# Memory Changelog Index Part 25

Source: `memory/CHANGELOG.md`

## 2026-03-17

- JIT TCO stale-generation ownership gating hardening:
  - `src/lisp/jit_jit_eval_scopes.c3`:
    - replaced env-chain recycle gate `scope_gen` matching with physical
      frame-in-scope detection (`boundary_ptr_in_scope(...)`) so recyclable
      fast-reset cannot skip copy/rebuild paths when frame stamps are stale
      after chunk transfer/move operations.
    - root-persistent parent rewrite gate now keys on physical parent residency
      in `releasing_scope` only (no early skip on stale generation stamps).
  - `src/lisp/eval_promotion_copy.c3`:
    - `copy_to_parent_try_fast_reuse(...)` now fails closed when either wrapper
      or closure/iterator payload still resides in `releasing_scope`, avoiding
      unsafe fast-reuse when wrapper provenance looks reusable but payload alias
      still points into releasing scope.
  - regression coverage:
    - added `run_jit_policy_tco_stale_generation_moved_binding_copy_test(...)`
      in `src/lisp/tests_runtime_feature_jit_groups.c3`.
    - added `run_jit_policy_tco_persistent_parent_stale_generation_rewrite_test(...)`
      in `src/lisp/tests_runtime_feature_jit_groups.c3`.
  - validation:
    - `c3c build --sanitize=address`
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp` (`22 passed, 0 failed`)

- Env-copy transactional frame materialization rollback hardening:
  - `src/lisp/eval_env_copy.c3`:
    - added transactional rollback helpers for partially materialized env-copy
      bindings (`copy_env_rollback_materialized_bindings(...)` and related
      value/iterator helpers).
    - binding-copy failure now rolls back already materialized target-scope
      wrapper side effects before returning `BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY`.
    - parent-copy failure after binding materialization now also rolls back
      materialized binding side effects before frame cleanup/return.
  - `src/lisp/tests_memory_lifetime_env_copy_groups.c3`:
    - added `run_memory_lifetime_env_copy_transactional_binding_rollback_test(...)`
      to assert mid-frame env-copy failure is transactional for
      closure/iterator/instance wrapper retain paths.
  - validation:
    - `c3c build --sanitize=address`
    - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp` (blocked by pre-existing `boundary_commit_escape` failures and ASAN UAF in `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3:147`; reproduces even with env-copy rollback callsites temporarily disabled)
    - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_VERBOSE=1 ./build/main --test-suite lisp` (`56 passed, 4 failed`; decoded pass output includes `[PASS] lifetime: env-copy transactional mid-frame rollback`; failures are the same pre-existing `boundary_commit_escape` cases)

- Unified closure/iterator alias safety policy across env-copy and promotion paths:
  - shared policy helpers in `src/lisp/eval_boundary_provenance.c3`:
    - `boundary_closure_alias_unsafe_for_reuse(...)`
    - `boundary_iterator_payload_alias_unsafe_for_reuse(...)`
    - these centralize undelimited closure / iterator thunk alias-safety checks
      against target-scope-chain residency.
  - `src/lisp/eval_env_copy.c3`:
    - undelimited closure alias guard now uses shared target-chain policy rather
      than releasing-scope-only detection.
    - iterator env-copy now rejects disjoint non-closure thunk payload aliases
      and only reuses iterator wrappers when both wrapper and payload aliasing
      are policy-safe.
  - `src/lisp/eval_promotion_copy.c3`:
    - fast-reuse gate now fail-closes on shared closure/iterator alias-unsafe
      conditions before reuse classification.
  - `src/lisp/eval_promotion_escape.c3`:
    - fast-path wrapper reuse now routes alias-unsafe closure/iterator values
      through disjoint promotion instead of preserving wrapper alias.
  - regression coverage:
    - added `run_memory_lifetime_env_copy_rejects_cross_scope_undelimited_alias_test(...)`
      in `src/lisp/tests_memory_lifetime_env_copy_groups.c3`.
    - added `run_jit_policy_cross_scope_alias_policy_test(...)`
      in `src/lisp/tests_runtime_feature_jit_groups.c3`.
  - validation:
    - `c3c build --sanitize=address`
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`23 passed, 0 failed`)
    - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp` (blocked by pre-existing `boundary_commit_escape` failures + ASAN UAF at `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3:147`)
    - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_VERBOSE=1 ./build/main --test-suite lisp` (`57 passed, 4 failed`; decoded pass lines include both new env-copy alias regressions and prior transactional rollback regression)

## 2026-03-16

- JIT/boundary fail-closed hardening follow-up:
  - `src/lisp/jit_jit_eval_scopes.c3`:
    - both call-scope wrappers now treat unresolved `pop_scope_guard_defer(...)` as fail-closed runtime errors (no silent pop failure ignore).
    - TCO env binding copy gating now checks releasing-scope residency by physical pointer provenance and wrapper payload aliasing (closure payload / iterator thunk payload), not only generation-stamp checks.
  - `src/lisp/jit_common.c3`:
    - `pop_scope_guard_defer(...)` now retries the explicit fallback stack context when undefer on the current active context fails.
  - `src/lisp/eval_boundary_commit_escape_builders.c3`:
    - destination ESCAPE builders (`cons`, `partial`, `iterator`, `error`) now materialize in a temporary child build scope and commit via `scope_splice_escapes(...)`; aborted/failed paths release the build scope to roll back partial objects.
  - `src/lisp/eval_boundary_api.c3`:
    - closure signature-copy failure (`BOUNDARY_COPY_FAULT_CLOSURE_TYPE_SIG_COPY`) now marks active promotion context(s) aborted before surfacing typed copy failure.
  - `src/lisp/jit_jit_closure_define_qq.c3`:
    - `jit_eval_set(...)` now relies on `env_set_with_barrier(...)` write-site promotion instead of unconditional pre-promotion to root.
    - instance field mutation now uses owner-scope boundary copy path uniformly (including root owner scope path).
  - `src/lisp/eval_promotion_root_store.c3`:
    - root-store clone builders (`array`, `hashmap/set`, `method-table`) now fail closed when nested `boundary_copy_to_parent_site_ctx(...)` returns an error value, and release partial heap allocations before returning the error.
    - this prevents boundary-copy fault values from being stored as normal payload entries inside root-store cloned containers.
  - validation:
    - `c3c build`
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`21 passed, 0 failed`)
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`basic` default; `139 passed, 0 failed`)
    - `LD_LIBRARY_PATH=/usr/local/lib:/usr/lib ./build/main --test-suite scope` (`58 passed, 0 failed`)
    - `LD_LIBRARY_PATH=/usr/local/lib:/usr/lib ./build/main --test-suite stack` (`23 passed, 0 failed`)
    - container memory-lifetime smoke slice run attempted via `scripts/run_validation_container.sh` but blocked by missing `libreplxx.so.0` in the validation image runtime.

- Env-copy persistent parent rewrite fail-closed hardening:
  - `src/lisp/eval_env_copy.c3`:
    - added explicit in-place rewrite eligibility guard for root-persistent env boxes (`copy_env_can_rewrite_persistent_box_in_place(...)`), requiring true root ownership and rejecting releasing-scope-resident "persistent" frames.
    - malformed persistent-tagged local env frames now fail closed with `BOUNDARY_ENV_COPY_FAULT_PARENT_COPY` instead of mutating transient frames in place.
    - added partial-frame cleanup on env-copy failure paths (`copy_env_cleanup_partial_frame(...)`) to release malloc-backed binding/hash buffers when frame materialization aborts before dtor registration.
    - added fail-closed guards for releasing-scope closure/iterator aliasing without `closure.env_scope` ownership envelope (`copy_env_invalid_closure_alias_in_releasing_scope(...)`), returning typed env-copy failure instead of preserving potentially stale wrapper pointers.
  - regression coverage:
    - added `run_memory_lifetime_env_copy_invalid_persistent_in_place_guard_test(...)` in `src/lisp/tests_memory_lifetime_env_copy_groups.c3`.
    - added `run_memory_lifetime_env_copy_rejects_undelimited_closure_alias_test(...)` in `src/lisp/tests_memory_lifetime_env_copy_groups.c3`.
  - validation:
    - `c3c build --sanitize=address`
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`20 passed, 0 failed`)
    - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp` (`59 passed, 0 failed`)

- Scheduler offload prestart-cancel ownership hardening:
  - `src/lisp/scheduler_offload_worker.c3`:
    - removed early `scheduler_release_shared(...)` from `scheduler_uv_offload_work_cb(...)` when `scheduler_try_begin_thread_task(...)` rejects execution.
    - shared payload release now stays single-authority in `scheduler_uv_offload_after_cb(...)` non-executed path, closing a double-release/UAF window on prestart-cancel interleavings.
    - added OOM fallback in executed task completion path: if completion materialization still fails, drop the task entry to avoid leaving it stranded in `THREAD_TASK_RUNNING`.
  - regression coverage:
    - added `run_scheduler_worker_prestart_cancel_single_shared_release_boundary_tests(...)` in `src/lisp/tests_scheduler_boundary_worker.c3`.
    - wired the regression in scheduler suite dispatch (`src/lisp/tests_scheduler_groups.c3`).
  - validation:
    - `c3c build --sanitize=address`
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`85 passed, 0 failed`)

- Scheduler thread-task completion fail-closed hardening:
  - `src/lisp/scheduler_thread_task_transitions.c3`:
    - added `scheduler_make_task_alloc_failure_completion(...)` and routed `scheduler_complete_thread_task(...)` null-completion input through alloc-failure completion materialization instead of early return.
    - if alloc-failure completion cannot be materialized, `scheduler_complete_thread_task(...)` now fail-closes by dropping the task entry (`scheduler_drop_thread_task(...)`) to avoid stranding `THREAD_TASK_RUNNING`.
  - regression coverage:
    - added `run_scheduler_thread_task_null_completion_fail_closed_boundary_tests(...)` in `src/lisp/tests_scheduler_boundary_thread_task_groups.c3`.
    - wired the regression in scheduler suite dispatch (`src/lisp/tests_scheduler_groups.c3`).
  - validation:
    - `c3c build --sanitize=address`
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`86 passed, 0 failed`)

- Scheduler waiter wakeup ownership hardening:
  - `src/lisp/scheduler_thread_tasks.c3`:
    - `scheduler_take_thread_task_completion(...)` and `scheduler_take_os_thread_completion(...)` now actively signal captured waiter fiber/epoch before entry teardown, preventing concurrent non-waiter completion-consume paths from clearing waiter linkage without wake delivery.
    - take paths no longer clear join tokens directly; wake handling now remains token-authoritative in wake dispatch.
  - `src/lisp/scheduler_thread_task_waiters.c3`:
    - `scheduler_clear_thread_task_waiters_for_fiber(...)` now also clears latched task/os-thread wake epochs for the fiber, removing stale latched wake residue on waiter teardown.
  - regression coverage:
    - added `run_scheduler_waiter_take_does_not_strand_blocked_fiber_boundary_tests(...)` in `src/lisp/tests_scheduler_boundary_thread_task_groups.c3`.
    - wired the regression in scheduler suite dispatch (`src/lisp/tests_scheduler_groups.c3`).
  - validation:
    - `c3c build --sanitize=address`
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`87 passed, 0 failed`)

- Scheduler/JIT continuation hardening follow-up:
  - scheduler stale-wakeup hardening:
    - added per-fiber `wake_epoch` stamps and scheduler-global `fiber_wake_epoch_seed`.
    - task/thread waiters now snapshot the waiter fiber wake epoch at waiter registration (`TaskEntry.waiter_wake_epoch` / `OsThreadEntry.waiter_wake_epoch`), and completion/cancel/drop paths publish that captured epoch instead of rereading current fiber table state.
    - task/thread completion wakeups now publish and validate wake epochs before unblocking a blocked fiber, preventing late waiter events from reviving a recycled fiber slot (including recycled slots from a newer interpreter generation).
    - completion teardown now clears lingering task/thread waiter slots for the finished fiber (`scheduler_clear_thread_task_waiters_for_fiber(...)`), removing stale waiter-id reuse windows.
  - scheduler handle ownership hardening:
    - task/thread handle payloads now record `owner_interp`.
    - task/thread handle parse paths now reject cross-interpreter use even when a handle wrapper is otherwise structurally valid.
  - JIT cache/runtime ownership hardening:
    - `JitCacheEntry` now records owner interpreter + attach serial.
    - owner-aware cache lookup/store now bind entries to the active interpreter epoch and fail closed on epoch mismatch (cache reset + recompile path), preventing stale cache reuse across interpreter serial transitions.
    - jit detach/shutdown paths no longer reset the global attach-serial counter; serial epochs remain monotonic across runtime idle/shutdown cycles.
    - retired-code tombstones are now pruned by attached-serial liveness during `jit_gc()` (`jit_retired_code_prune_detached_serials()`), avoiding detach-before-gc prune ordering hazards.
  - continuation lifecycle hardening:
    - `Continuation` now tracks `owner_interp` and is registered on an interpreter-owned intrusive list.
    - interpreter teardown now invalidates tracked continuations and releases retained handle state snapshots before root-scope teardown (`interp_invalidate_live_continuations(...)`), preventing escaped handle-state retention from outliving interpreter lifetime.
    - continuation boundary copy/promotion now rejects cross-interpreter continuation aliasing (`copy_continuation_to_parent`, `promote_escape_continuation`).
    - continuation resume/resolve now additionally enforce `owner_interp` checks before proceeding.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`

- Continuation teardown UAF hardening (ASAN follow-up):
  - fixed a use-after-free in interpreter teardown where continuation list entries
    could still hold stale `HandleEffectState*` pointers and
    `interp_invalidate_live_continuations(...)` dereferenced freed memory.
  - `src/lisp/jit_jit_handle_signal.c3`:
    - added a live-state registry for `HandleEffectState` allocations:
      - `g_handle_effect_live_head`,
      - `handle_effect_state_register_live(...)`,
      - `handle_effect_state_unregister_live(...)`,
      - `handle_effect_state_is_live(...)`.
    - `handle_effect_state_init(...)` now registers live states.
    - `handle_effect_state_release(...)` now:
      - no-ops on non-live/stale pointers,
      - unregisters live state before free,
      - scrubs continuation back-references before releasing buffers and state.
  - `src/lisp/value_interp_lifecycle.c3`:
    - `interp_invalidate_live_continuations(...)` now gates all
      `HandleEffectState` dereferences/releases on `handle_effect_state_is_live(...)`,
      so stale pointers are nullified without touching freed memory.
  - regression coverage:
    - added `run_jit_policy_shared_handle_state_teardown_test(...)` in
      `src/lisp/tests_runtime_feature_jit_groups.c3` to exercise retained-first
      / non-retained-second shared-handle-state teardown order.
  - validation:
    - `c3c build --sanitize=address`
    - `OMNI_LISP_TEST_SLICE=basic LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`139 passed, 0 failed`)
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`19 passed, 0 failed`)

- Stack-pool shutdown UAF hardening (scheduler ASAN follow-up):
  - fixed cross-context teardown ordering in `src/stack_engine_pool_ownership.c3`:
    - `stack_pool_shutdown(...)` now runs all deferred/lifecycle callbacks for
      all owned contexts before freeing any `StackCtx` allocation.
    - this closes a UAF window where a later context's deferred scope release
      could touch a coroutine `StackCtx*` already freed earlier in the same
      shutdown pass.
  - regression coverage:
    - added `test_stack_pool_shutdown_defer_cross_ctx_order(...)` in
      `src/stack_engine_tests_defer.c3` and wired it in
      `src/stack_engine_tests_runner.c3`.
  - validation:
    - `c3c build --sanitize=address`
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`84 passed, 0 failed`)
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=basic LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`139 passed, 0 failed`)
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack` (`22 passed, 0 failed`)

- Handle-state continuation scrub hardening:
  - `src/lisp/jit_jit_handle_signal.c3`:
    - `handle_effect_state_scrub_continuations(...)` now also clears
      `Continuation.ctx` when scrubbing continuations bound to a released
      `HandleEffectState`, forcing later resolve paths to fail closed instead of
      touching stale stack-context pointers.
  - regression coverage:
    - added `run_jit_policy_side_effect_escaped_handle_continuation_guard_test(...)`
      in `src/lisp/tests_runtime_feature_jit_groups.c3`, asserting released
      handle-state continuations are scrubbed to unusable state (`ctx == null`,
      `handle_state == null`, `handle_retained == false`).
  - validation:
    - `c3c build --sanitize=address`
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`20 passed, 0 failed`)
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`84 passed, 0 failed`)

- JIT/env-copy persistent-parent lifetime hardening:
  - `src/lisp/eval_env_copy.c3`:
    - root-persistent mutable-box parent rewrites are now conditional (`copy_env_parent_needs_rewrite(...)`) and only run when the direct parent still points into the releasing scope.
    - when a rewrite is required, parent-chain materialization now executes with `interp.current_scope = interp.root_scope` so persistent-box parent links no longer borrow lifetime from transient target scopes.
  - `src/lisp/jit_jit_eval_scopes.c3`:
    - `copy_tco_env_chain(...)` now applies the same conditional parent-rewrite gate for root-persistent boxes and rewrites via root-scope materialization.
    - TCO recycle in-place TEMP reset now requires conservative detach eligibility:
      - no root-persistent boxes in the traversed env chain,
      - no graph-carrying binding values (`CONS/ARRAY/HASHMAP/SET/CLOSURE/MODULE/PARTIAL_PRIM/METHOD_TABLE/ITERATOR/INSTANCE`).
    - this closes the shallow-detach fast-path window where nested reachable graphs could retain TEMP-lane edges across `scope_reset_temp_lane(...)`.
  - regression coverage:
    - updated env-copy lifetime regressions in `src/lisp/tests_memory_lifetime_env_copy_groups.c3` to assert rewritten persistent parents are root-owned and survive both source-scope and target-scope release without manual parent detachment.
    - added JIT policy regression `run_jit_policy_tco_persistent_parent_root_rewrite_test(...)` in `src/lisp/tests_runtime_feature_jit_groups.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`19 passed, 0 failed`)
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`81 passed, 0 failed`)

- Scheduler join-wakeup stale-epoch hardening:
  - replaced join waiter wake matching from coarse fiber lifetime epochs to per-registration join tokens.
  - `scheduler_register_thread_task_waiter(...)` / `scheduler_register_os_thread_waiter(...)` now mint monotonic scheduler-global join tokens and arm them on the waiter fiber (`Scheduler.fiber_join_wait_token[...]`).
  - task/thread completion wakeups now carry that registration token and dispatch validates against the currently armed token before waking blocked fibers.
  - wake dispatch consumes the armed join token on match, preventing delayed duplicate task/thread wake events from reviving later unrelated waits.
  - join token lifecycle is now explicitly reset across:
    - waiter clear paths (`scheduler_clear_thread_task_waiter`, `scheduler_clear_os_thread_waiter`, `scheduler_clear_thread_task_waiters_for_fiber`),
    - completion consume paths (`scheduler_take_thread_task_completion`, `scheduler_take_os_thread_completion`),
    - fiber slot reuse/reset (`scheduler_add_fiber`, `scheduler_reset_wakeup_queue`).
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`81 passed, 0 failed`)
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`19 passed, 0 failed`)

- Continuation/suspend ownership guard follow-up:
  - `suspend_with_scope_guard(...)` now only performs the post-resume scope release when defer-pop succeeds, preventing retain over-release on unexpected pop failure paths.
  - continuation resume hardening now rejects cross-interpreter continuation application/resolution by validating `StackCtx.owner_pool` (and handle-state interpreter when present) against the active interpreter.
  - continuation resolve/teardown paths now destroy stack contexts through `ctx.owner_pool` authority (with local fallback), removing pool-mismatch destroy hazards.
  - `capture` suspend failure paths now restore saved interpreter state before returning error in both AST and value runtime-effect paths.
  - fixed scope-guard defer pop routing to prefer the currently running `StackCtx`, preventing cloned multi-shot continuation resumes from accidentally popping defer state on the source template context.
  - handle dispatch paths now conservatively retain handler state/context before frame cleanup when a continuation is reachable from the handler result graph, preventing dangling `handle_state`/suspended-context use-after-free on escaped continuation wrappers.
  - escaped handle continuations with inactive frames are now rejected deterministically (`runtime/continuation-frame-inactive`) and invalidated with deterministic teardown instead of reaching unsafe resume paths.
  - added JIT policy regression coverage for cross-interpreter continuation resume rejection.
  - added JIT policy regression coverage ensuring escaped handle continuations are rejected without corrupting evaluator state.
  - added JIT policy regression coverage for multi-shot capture clone isolation (`(checkpoint ... (capture ...))` reused twice) to guard scope-retain/defer correctness across cloned continuation resumes.
  - `SavedInterpState` now snapshots/restores `g_jit_exec_depth` across stack-context switches so nested checkpoint/capture resumes cannot corrupt global JIT frame-depth accounting.
  - fixed `jit_signal_suspend(...)` to use a stack-local `SavedInterpState` instead of scope-allocation-backed storage, removing a null-dereference hazard on allocation failure paths.
  - fixed `jit_resolve_value(...)` to snapshot/restore interpreter boundary state around continuation resume (`stack_ctx_switch_to`), preventing resumed resolve paths from leaking transient JIT/eval state into the caller.
  - fixed `current_reset_state` restoration ordering in checkpoint/value-checkpoint/continuation-resume switch paths so boundary restore no longer reintroduces stack-local reset-state pointers after return.
  - hardened `capture` suspend-failure paths (AST + value) to clear staged `shifted` continuation metadata before returning error, preventing false parent-side shift handling when suspend registration fails.
  - continuation overflow exit now clears one-shot `resume_value` before returning error, preventing stale resume payload leakage into later resumes.
  - hardened direct JIT binop specialization (`+/-/*/< />/=`) to require unshadowed canonical primitive bindings, preventing inline fast-path compilation from bypassing local/global rebinding semantics.
  - added JIT runtime thread-affinity ownership checks over global JIT state/caches, causing immediate deterministic failure on cross-thread JIT runtime access instead of silent global-state races.
  - hardened JIT compiled-function lifetime handling by tracking emitted code pointers alongside libjit states and validating liveness before invocation (`jit_exec`/`jit_eval`), so stale `JitFn` values are rejected with a runtime error after `jit_gc()` teardown instead of jumping into freed code.
  - hardened JIT suspend/GC interaction by tracking active suspend scope-guards (`suspend_with_scope_guard` + clone/destroy lifecycle), and blocking `jit_gc()` / full `jit_global_shutdown()` teardown while suspended JIT frames are still reachable.
  - hardened JIT compiled-function ownership by tracking source-interpreter ownership per compiled state and requiring caller-interpreter ownership + live attachment at execution; compiled pointers from detached/foreign interpreters now fail closed with deterministic runtime error.
  - expanded stack-switch state snapshots (`SavedInterpState`) to include pending-raise payload/message and boundary metadata (`releasing_scope`, `active_promotion_ctx`), and added a shared `restore_interp_state_preserve_raise(...)` helper so continuation/coroutine/effect resume paths do not silently drop pending raise state.
  - fixed value/AOT handle runtime path to dispatch pending `raise` clauses after body return (parity with AST handle path), preventing installed raise handlers from being bypassed as raw error returns.
  - added JIT policy regression coverage asserting shadowed arithmetic symbols do not route through direct primitive fast-path specialization.
  - added JIT policy regression coverage asserting checkpoint boundaries do not leak reset-state pointers into later capture calls.
  - added JIT policy regression coverage asserting stale compiled function pointers are rejected after explicit `jit_gc()` safe-point teardown.
  - added JIT policy regression coverage asserting compiled pointers are rejected after owner-interpreter teardown when another interpreter keeps global JIT runtime alive.
  - JIT policy multi-shot continuation clone regression now skips when JIT checks are disabled (ASAN runtime), matching intentional sanitizer-mode one-shot continuation behavior.
  - hardened attached-interpreter ownership tracking with per-attachment serial epochs (stored on tracked JIT states/spill nodes), so pointer-address reuse across interpreter teardown/re-init cannot satisfy compiled-function liveness checks by raw `Interp*` identity alone.
  - fixed value/AOT `handle` stack-switch parity by snapshotting/restoring `SavedInterpState` around `stack_ctx_switch_to(...)`, aligning parent-side handler dispatch state with AST handle runtime paths.
  - hardened handle-state capture copies to reject promoted non-closure/error values from `boundary_promote_to_root(...)` before storing into escaped handler-copy closure tables.
  - hardened continuation resume flow to:
    - propagate blocked-fiber yields while a resumed continuation clone remains suspended,
    - reject unresolved suspended-clone completion paths deterministically (instead of consuming stale/null results as if completed).
  - hardened coroutine `resume` input validation to reject non-resumable context states before `stack_ctx_resume(...)`, preventing stack-engine assertion aborts on invalid states.
  - hardened scheduler fiber-blocking operations (`await`, `offload`, task/thread joins, async tcp read/write bridge) to require execution from the active fiber root stack context; nested stack contexts now fail closed with `scheduler/fiber-nested-blocking-op` instead of silently blocking the outer fiber while suspending only an inner context.
  - added scheduler regression coverage for nested-context `await` inside a fiber (`checkpoint` nested stack context), asserting deterministic rejection with the new root-context guard.
  - added retired-code tombstone bookkeeping keyed by attach serial and a deterministic prune path (`forget_serial`) plus detached-interpreter GC scheduling; this hardens JIT lifetime accounting without changing compile success behavior.
  - hardened JIT detach/destroy lifecycle guards for active on-stack interpreter teardown (`jit_interp_on_current_stack(...)`) while preserving suspended-continuation teardown paths used by existing continuation tests.
  - added JIT policy regression coverage for retired-code tombstone tracking and per-serial tombstone pruning.
  - hardened effect `resolve` continuation semantics:
    - one-shot continuation is now consumed before resume to fail closed on re-entrant reuse from resumed code,
    - suspended continuation resume now uses `stack_ctx_resume(...)` (status parity) plus blocked-fiber drive parity,
    - still-suspended completion paths now fail closed with deterministic `runtime/resolve-resuspended` errors.
  - hardened value/AOT checkpoint and handle boundaries to propagate blocked-fiber yields and reject unresolved suspended returns when no escaped continuation owns the frame.
  - hardened scheduler run-loop reentrancy:
    - `run-fibers` and run-loop entrypoints now reject nested invocation with `scheduler/reentrant-run-loop`,
    - added scheduler regression coverage asserting nested `(run-fibers)` inside an active fiber is rejected.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`

- Coroutine wrapper boundary ownership hardening:
  - `copy_coroutine_to_parent(...)` now enforces transfer semantics for coroutine wrappers instead of shallow aliasing:
    - only releasing-scope-owned wrappers are transferable,
    - destination wrapper is materialized with a registered value destructor,
    - source wrapper is nulled after transfer so scope teardown cannot double-own/double-destroy the same `StackCtx*`.
  - `promote_escape_coroutine(...)` now enforces the same single-owner transfer policy for TEMP->ESCAPE promotion in the current scope:
    - rejects disjoint wrapper aliasing attempts,
    - registers ESCAPE-lane destructor coverage for transferred coroutine wrappers,
    - nulls the source TEMP wrapper after transfer.
  - added memory-lifetime regressions in `src/lisp/tests_memory_lifetime_finalize_groups.c3`:
    - `promote_to_root` coroutine-wrapper transfer preserves resumability and single teardown,
    - `promote_to_escape` coroutine-wrapper transfer preserves single teardown on scope release.
  - validation:
    - `c3c build`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack`
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `./scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=memory-lifetime-smoke LD_LIBRARY_PATH=build:/usr/local/lib:deps/lib ./build/main --test-suite lisp`

- Stack/effect boundary follow-up hardening:
  - stack context pool safety:
    - added idempotency guard for repeated `stack_ctx_destroy(...)` on already pooled contexts (`StackCtx.in_pool_cache`), preventing free-list self-cycles/corruption from double-destroy paths.
    - added stack-engine regression `pool double-destroy idempotent`.
  - stack guard infrastructure lifetime:
    - `csrc/stack_helpers.c` now reference-counts global guard initialization/shutdown so one live pool shutdown no longer tears down SIGSEGV guard infrastructure for other live pools/interpreters.
  - coroutine resume/cleanup ownership authority:
    - `resume` now rejects coroutine wrappers whose `StackCtx` does not belong to the active interpreter/pool.
    - coroutine cleanup now destroys through `ctx.owner_pool` authority (with local fallback), aligning with destructor ownership routing.
  - JIT saved-state parity:
    - `SavedInterpState` now includes `current_reset_state`, so handle/effect stack switches preserve reset/capture state instead of restoring a stale/foreign pointer.
  - handle-state copy safety:
    - `handle_effect_state_capture_handler_copy(...)` no longer shallow-copies source-owned pointers before allocation; partial-allocation failures now clean only locally-owned buffers, eliminating invalid-free of caller-owned handler arrays.

- CLI parse/check surface for tooling:
  - added `omni --check <file>` as a non-executing parse/check mode for Omni source files.
  - added `omni --check --json <file>` structured diagnostics output with:
    - stable diagnostic code (`parser/syntax-error` and `io/read-failed` in the initial pass),
    - documented 0-based JSON ranges,
    - exact input path,
    - non-zero exit on diagnostics or read failure.
  - intent:
    - give editor tooling a first-party, non-executing diagnostics surface,
    - stop depending on regex parsing of human `--compile` output for basic syntax checks,
    - keep the first implementation parse-only so arbitrary buffers can be checked safely.
  - follow-up:
    - extend the same contract toward richer analysis and structured eval transport.

- CLI structured eval surface for tooling:
  - added `omni --eval '<expr>'` for single-expression CLI evaluation.
  - added `omni --eval --json '<expr>'` structured result output with:
    - echoed input,
    - rendered value string on success,
    - structured runtime error payload on failure,
    - 0-based source range when line/column are available.
  - intent:
    - provide a stable machine-readable eval primitive for editor tooling,
    - avoid scraping colored REPL output for one-shot eval requests,
    - keep the first transport small before attempting multi-request REPL protocol work.

- Structured session transport for editor tooling:
  - added `omni --repl --json` as a newline-delimited JSON request/response transport.
  - request shape in the initial pass:
    - `{"id":"...","input":"...","mode":"expr"}` for single-expression eval,
    - `{"id":"...","input":"...","mode":"program"}` for multi-form buffer eval.
  - response shape includes:
    - echoed `id`,
    - `ok`,
    - rendered value string on success,
    - structured error payload on failure.
  - intent:
    - keep a persistent Omni process for editor integrations,
    - remove ANSI-text scraping from repeated REPL-driven requests,
    - support whole-buffer program sends without inventing a second plugin-side protocol.
  - first consumer:
    - `tooling/omni-nvim` now defaults to `omni --repl --json` for persistent session traffic,
    - whole-buffer sends and REPL fallback eval paths no longer depend on PTY text scraping.
    - current-form eval in `tooling/omni-nvim` now prefers Tree-sitter form selection when an Omni parser is available, with the previous delimiter scanner retained as fallback.
    - `tooling/omni-nvim` now also exposes transcript clear + auto-scroll control and visual selection eval over the structured session transport.
    - `tooling/omni-nvim` now exposes root-form eval from the cursor, using Tree-sitter when available and delimiter-based top-level form scanning as fallback.
    - `tooling/omni-nvim` now registers the Omni `nvim-treesitter` parser config from the current repo checkout when `nvim-treesitter` is available, and adds `tooling/tree-sitter-omni` to `runtimepath` so bundled queries resolve without manual copying.
    - `tooling/omni-nvim` now exposes `:OmniLspSetup` plus optional `lsp.auto_setup = true`, registering the first-party `omni-lsp` server from the current repo checkout through Neovim's built-in LSP API when available and `lspconfig` as fallback.
    - `tooling/omni-nvim` now also exposes buffer-local `OmniLsp*` actions and default mappings for hover, definition, declaration, implementation, type definition, references, rename, code action, formatting, and signature help, delegating to `vim.lsp.buf.*` in Omni buffers.
    - `tooling/omni-nvim` now exposes quickfix-backed `OmniLspDocumentSymbols` and `OmniLspWorkspaceSymbols` commands plus default mappings, using synchronous LSP requests to surface current-buffer and workspace symbol lists from the attached Omni LSP client.
    - `tooling/omni-nvim` now exposes quickfix-backed `OmniLspDefinitionsList`, `OmniLspDeclarationsList`, `OmniLspImplementationsList`, and `OmniLspTypeDefinitionsList` plus default mappings, using synchronous location requests to surface multi-target navigation results from the attached Omni LSP client.
    - `tooling/omni-nvim` now exposes quickfix-backed `OmniLspReferencesList` plus a default buffer-local mapping, using a synchronous `textDocument/references` request to list current-symbol references from the attached Omni LSP client.
    - `tooling/omni-nvim` now exposes diagnostics helpers and default mappings for opening the current buffer location list and jumping to the next or previous diagnostic through Neovim's built-in diagnostic API.
    - `tooling/omni-nvim` now exposes inlay-hint enable/disable/toggle commands plus a default buffer-local toggle mapping, delegating to Neovim's built-in inlay-hint API for the current Omni buffer.
    - `tooling/omni-nvim` now exposes `:OmniLspRefreshFolds` plus a default buffer-local mapping, requesting `textDocument/foldingRange` from the attached Omni LSP client and applying the returned ranges as manual folds in the current window.
    - `tooling/omni-nvim` now exposes `:OmniLspExpandSelection` plus a default buffer-local mapping, requesting `textDocument/selectionRange` and expanding the current Omni selection outward through the returned parent chain one structural step at a time.
    - `tooling/omni-nvim` now forwards explicit Ex ranges and visual selections through `OmniLspCodeAction` and `OmniLspFormat`, so selection-based code actions and formatting use the actual Omni region instead of only cursor-local behavior.
    - `tooling/omni-nvim` now exposes operator-style eval over Vim motions, routing motion-selected regions through the same structured program-mode transport as visual selection eval.
    - `tooling/omni-nvim` now exposes capture-aware eval for enclosing declaration, call, and block regions, driven by the first-party Omni Tree-sitter textobject queries with the existing form/root selection paths kept as fallback.
    - `tooling/omni-nvim` now exposes structural selection commands and default mappings for enclosing form, root form, declaration, call, and block regions, reusing the same capture-aware range lookup as eval.
    - `tooling/omni-nvim` now exposes buffer-local Omni textobjects in operator-pending and visual mode for form, root form, declaration, call, and block selection, with configurable keys and a separate enable/disable switch.
    - `tooling/omni-nvim` now distinguishes inner and outer textobjects: inner selections trim bracket/prefix wrappers when possible, while outer selections keep the full structural form.
    - `tooling/omni-nvim` now loads the bundled Omni query files directly into Neovim when no existing Omni query is registered for that group, so first-party capture-driven behavior does not depend on a second query packaging step.
    - `tooling/tree-sitter-omni` now ships first-party textobject and fold queries for structural Omni forms, including call-shaped lists, block-style special forms, shorthand function definitions, module/type/effect declarations, and form comments.

- LSP baseline upgrade for editor navigation:
  - `tooling/omni-lsp` now advertises and serves `textDocument/documentSymbol`.
  - document symbols are extracted from current-buffer top-level Omni declarations, including:
    - plain `define` bindings,
    - function shorthand declarations,
    - attribute-backed declarations such as `[type]`, `[abstract]`, `[union]`, `[alias]`, `[effect]`, and related declaration families,
    - module declarations with nested child symbols for body-local declarations.
  - completion now merges current-buffer declaration names ahead of static builtins/special forms.
  - workspace symbol search now returns syntactic declaration matches across:
    - currently open Omni documents known to the LSP server,
    - unopened `.omni` files under workspace roots discovered from `project.json` or `.git` markers near open documents.
  - workspace symbol search still keeps module container names for nested declarations and skips common generated/dependency directories during the file scan.
  - workspace-root symbol scans are now cached in-process behind a path/mtime/size manifest:
    - unchanged unopened `.omni` files are reused across repeated `workspace/symbol` queries,
    - changed unopened files are reread when their filesystem signature changes,
    - document open/change/close events still clear the cache eagerly.
  - unchanged unopened workspace files now also reuse cached parsed declaration summaries, so repeated `workspace/symbol` queries skip both file rereads and declaration rescans for those files.
  - local go-to-definition now resolves current-buffer top-level/module-body declarations and returns multiple targets when a symbol name has more than one declaration.
  - go-to-definition now falls back to exact-name declaration matches from open and unopened workspace Omni files when the active buffer has no local declaration for that symbol.
  - `textDocument/declaration` is now exposed and currently follows the same local-first, exact-name workspace fallback path as definition.
  - `textDocument/implementation` is now exposed and currently follows the same local-first, exact-name workspace fallback path as definition.
  - `textDocument/typeDefinition` is now exposed for type-like declarations (`type`, `abstract`, `union`, `alias`) and follows the same local-first, exact-name workspace fallback path.
  - hover now falls back to source-backed exact-name declaration snippets from open and unopened workspace Omni files when the active buffer has no local declaration for that symbol.
  - completion now appends declaration names from other open and unopened workspace Omni files after current-buffer declarations, reusing the cached workspace declaration summaries instead of rescanning unchanged unopened files.
  - signature help is now available through a syntactic call-form scan:
    - current-buffer function declarations are used first,
    - exact-name workspace function declarations from open and unopened Omni files are used as fallback,
    - a small static set of core special forms also exposes signatures.
  - parameter inlay hints now reuse the same local/workspace declaration data and only emit hints when the resolved function name has one unambiguous parameter label set, avoiding guesses for conflicting overloads.
  - folding ranges are now exposed from a structural multiline-form scan, covering modules, multiline declarations, `block` forms, and nested multiline list/vector/map forms without semantic indexing.
  - selection ranges are now exposed from the same structural form scan, expanding from the symbol under the cursor out through enclosing nested Omni forms for editor expand-selection support.
  - document and range formatting are now available as a conservative indentation pass that normalizes leading indentation and trims trailing whitespace without attempting full pretty-printing or intra-line spacing rewrites.
  - local code actions now offer declaration rewrites for:
    - shorthand function defines to explicit lambda bindings,
    - explicit lambda bindings back to shorthand function defines,
    - wrapping multi-form function bodies in an explicit `block`,
    - inlining redundant explicit `block` bodies back into direct declaration forms.
  - hover now returns source-backed snippets for current-buffer declarations, including overloaded local names, before falling back to static builtin/special-form docs.
  - document highlights now mark current-buffer declaration names and their local uses, classifying declaration sites as write highlights and ordinary occurrences as read highlights.
  - local references and rename now work for current-buffer declaration names, including overloaded names, by reusing the same syntactic declaration scan and exact symbol-range extraction.
  - intent:
    - give editors a first practical navigation surface without waiting for semantic indexing,
    - surface module-body declarations in common Omni source layouts,
    - keep the implementation syntactic and local to the active buffer for predictable behavior.
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`

- JIT cache overwrite accounting fix:
  - fixed `jit_cache_store(...)` so overwriting an existing `Expr*` cache entry no longer increments `g_jit_cache_count`.
  - hardened `jit_cache_store(...)` so overwrite paths do not trigger false-positive full-cache clears when `g_jit_cache_count` is already at the threshold.
  - hardened saturated 16-slot probe windows to clear-and-retry once instead of silently dropping the compiled entry.
  - this prevents false-positive cache saturation and premature full-cache clears from repeated store paths over the same compiled expression.
  - added a focused JIT-policy regression test asserting:
    - first store of one AST increases cache count to `1`,
    - second store of the same AST keeps cache count at `1`.
    - overwrite at the cache threshold preserves existing entries instead of clearing the cache.
    - a saturated local probe window still stores the target expression after a clear-and-retry recovery path.
  - motivation:
    - `omni-torch/examples/xor_nn.omni` exposed noisy `[debug] JIT cache full (...), clearing` churn during a long-running effect/JIT workload,
    - root cause included cache-entry overwrite accounting and cache-store edge handling, not actual unique-entry growth on that path.
  - validation:
    - `c3c build` passed.
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=build:/usr/local/lib:deps/lib ./build/main --test-suite lisp` confirms the new cache regressions pass.
    - note: the same `jit-policy` slice still has a separate pre-existing failure in `gc runs only at explicit safe points`; this change does not touch that path.

- Scheduler boundary hardening + AOT build command safety:
  - `scheduler_add_fiber(...)` now validates coroutine shape after `boundary_promote_to_root(...)` as well as before promotion, so non-null non-coroutine promotion results are rejected deterministically.
  - scheduler offload start now marks the pending slot active before enqueueing worker work and rolls state back on enqueue failure, removing a race where a fast offload-ready wakeup could arrive before slot activation and get dropped as invalid.
  - scheduler fallback errors used when root promotion fails are now allocated in `root_scope` to avoid storing current-scope error values into long-lived fiber result fields.
  - interpreter teardown now clears global JIT cache before AST arena destruction so `Expr*` cache keys cannot survive a destroyed/reused arena.
  - sequential top-level `spawn`/`await` paths now reclaim idle-complete scheduler batches (same reset policy already used by `run-fibers`), preventing false `scheduler full` after long non-`run-fibers` spawn loops.
  - AOT build backend command assembly now shell-quotes the output path and fails fast on command-buffer overflow, hardening `--build -o ...` against shell metacharacter injection/truncation behavior.
  - AOT build temp C3 source now uses per-process unique `build/_aot_temp_<pid>_<seq>.c3` paths (exclusive-create mode) and is deleted after compile, removing fixed-temp-path clobber races.
  - `--build` now fails fast on malformed `-o` usage with a missing output operand instead of silently falling back to a derived default output path.
  - `--build` now resolves the repo root from the running executable and executes the build pipeline from that root, so AOT helper/script + source-glob paths no longer depend on the caller’s current working directory.
  - `--build` now preserves caller-relative `input` / `-o` paths by normalizing them to absolute paths before switching to repo-root execution.
  - AOT backend compile path now lives fully in `entry_build_backend_compile.c3` without dependency on `entry.c3`-local extern declarations, keeping `--build` backend wiring self-contained.
  - AOT backend compile execution no longer uses shell-string `system(...)`; `--build` now runs helper/build commands through subprocess spawning with argv vectors, eliminating shell interpolation of caller-controlled paths.
  - AOT backend source globs are now expanded in-process (`src/main*`, `src/scope_region*`, `src/stack_engine*`, `src/lisp/*.c3`, `src/pika/*.c3`) before invoking `c3c`, so compile inputs are explicit argv entries instead of shell-expanded patterns.
  - AOT backend now pins `c3c` resolution to verified absolute paths (`/usr/local/bin/c3c`, `/usr/bin/c3c`, `/bin/c3c`, `/opt/homebrew/bin/c3c`) and fails closed when none exist, removing PATH-based compiler lookup in `--build`.
  - AOT backend now verifies required local runtime files (`./scripts/build_omni_chelpers.sh`, `/usr/bin/env`) before spawn, failing early with explicit diagnostics when prerequisites are missing.
  - AOT backend source list remains scoped to `src/main*.c3` + runtime/lisp sources (without pulling CLI `entry.c3`), avoiding duplicate generated-vs-cli `main` symbol collisions in `--build` output compilation.
  - JSON REPL transport in `src/lisp/eval_repl_json.c3` now uses a local JSON-string escaping helper instead of `main::print_json_escaped_string`, removing backend coupling to CLI reporting modules in AOT compile paths.
  - stack-affinity harness now captures probe output via `popen(... 2>&1)` instead of redirecting to predictable `/tmp` log files, removing fixed log-path spoof/clobber behavior.
  - stack-affinity harness probe-status validation now treats any non-zero wait status (exit or signal) as a probe failure, avoiding false negatives when the subprocess terminates by signal.
  - compiler bindgen canonical-type regression test now uses pid+sequence unique `/tmp` header/output paths instead of fixed filenames.
  - scheduler cancel cascade now re-checks parent state after recursive child cancellation to avoid double-completing a parent that was already completed via `waiting_children` resolution.
  - scheduler cancel completion now uses `scheduler_complete_fiber(...)` (not direct `scheduler_mark_done(...)`) so canceled parents with live descendants stay in `waiting_children` until child accounting reaches zero.
  - scheduler fibers now track `cancel_requested`, and `waiting_children` completion paths now prefer cancellation results when a parent was explicitly canceled, preventing child-cascade completion from preserving stale success `pending_result` values.
  - JIT runtime teardown is now guarded by live-interpreter reference counting: `Interp.init()` attaches and `Interp.destroy()` detaches, and `jit_global_shutdown()` now skips full runtime teardown while any interpreter remains alive.
  - this prevents one interpreter destroy path from invalidating JIT state/code buffers still reachable from other live interpreters.
  - stack-pool shutdown now sweeps all pool-owned `StackCtx` allocations (active + free-list), not only cached free-list entries, so interpreter teardown reclaims abandoned suspended continuation/coroutine stacks deterministically.
  - stack-pool ownership now tracks an intrusive all-context list (`all_list`/`all_count`) in addition to the recycle free list, and `stack_ctx_destroy(...)` now unlinks contexts when they are permanently freed outside the pool cache.
  - `Interp` table-growth paths (`macro`/`module` hash rebuild and effect handler stack growth) now assert on allocation failures before swapping live storage, removing unchecked-null dereference/corruption paths during growth.
  - `--build` absolute-path normalization now returns explicit `input/output path too long` errors instead of crashing on `io::bprintf(... )!!` unwrap for oversized caller paths.
  - default `--build` output derivation now trims extension dots only in the basename segment, avoiding incorrect truncation when parent directories contain dots.
  - default `--build` output derivation now fails fast if the derived path does not fit the output buffer, instead of silently truncating the output location.
  - added scheduler regression coverage asserting offload setup failure rolls pending slot + fiber state back to pre-enqueue values.
  - added scheduler regression coverage for `>MAX_FIBERS` sequential spawn+await loops without `run-fibers`, asserting idle-capacity reclamation.
  - added scheduler regression coverage for `fiber-cancel` child-cascade paths to ensure parent completion is not applied twice.
  - added scheduler regression coverage ensuring parent cancel remains `waiting_children` (with cancel result staged) when a descendant is currently `RUNNING` and cannot be canceled immediately.
  - strengthened scheduler cancel-cascade regression to assert canceled parent result type is `ERROR` (cancellation) instead of preserving prior success `pending_result`.
  - added JIT policy regression coverage for multi-interpreter lifetime: destroying a secondary interpreter must not tear down JIT runtime while a primary interpreter still executes previously compiled JIT code.
  - added JIT policy regression coverage for interpreter teardown with an escaped continuation (`(checkpoint (capture k k))`), asserting stack-pool ownership count drains to zero after destroy.
  - tightened compiler multi-arg lowering regression to require `invoke_once` in intermediate-argument lowering instead of passing on generic `invoke` presence.

- Quasiquote and continuation boundary hardening:
  - removed fixed-size 17-element quasiquote call flattening in both compiler and JIT expansion paths; wide quasiquote call forms now iterate directly over `func` + `args` instead of copying through a small stack buffer.
  - fixed `resume_value` consumption so coroutine/yield and JIT continuation/effect resume paths clear the one-shot resume payload after reading it.
  - fixed `jit_compile(...)` to stop running `jit_gc()` implicitly; JIT GC remains an explicit top-level safe-point action.
  - added regressions for:
    - wide quasiquote list evaluation and code generation,
    - coroutine implicit-`nil` resume after an earlier explicit resume value,
    - stale `resume_value` not leaking from `resolve` or `capture` into a later coroutine resume,
    - `jit-policy` safe-point behavior now matching explicit-GC-only intent.

- Serializer and CLI/test-runner consistency fixes:
  - fixed expression serializer output for parser-owned forms so emitted surface syntax now round-trips for:
    - `import` with string-path targets,
    - selective `import` aliases,
    - `handle` clauses,
    - `match` clauses.
  - bare `omni` now honors the documented default mode and starts the REPL instead of dispatching into the test runner.
  - Lisp/compiler test failures now return normal nonzero process status through the CLI test path instead of aborting via `assert(...)`.
  - added focused compiler regressions covering serializer round-trips for `import`, `handle`, and `match`.
