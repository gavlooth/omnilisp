# Active TODO Index Part 11

Source: `TODO.md`

- [x] `AUDIT-JIT-MULTI-ARG-LIST-FAILCLOSED-032` make JIT multi-arg call
  construction and iterative apply fail closed on malformed arg-list state
  instead of degrading to partial success
  - closure evidence:
    - `src/lisp/jit_jit_apply_runtime.c3`
      now rejects `make_cons(...)` failure while constructing continuation-safe
      multi-arg call lists instead of passing malformed arg lists downstream.
    - `src/lisp/jit_jit_apply_multi_prims.c3`
      now makes `jit_apply_multi_args_iterative(...)` return
      `"arg list too short"` when the arg list breaks before all required args
      are consumed, instead of breaking and returning the partial result.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now directly pins the malformed multi-arg list case in `jit-policy`
      using a two-arg curried closure and a one-element arg list.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=multi-arg-malformed-list-fails-closed ./build/main --test-suite lisp'`

- [x] `AUDIT-TWO-ARG-LIST-MATERIALIZATION-FAILCLOSED-031` make shared
  two-value list/arg materialization fail closed instead of publishing cons
  constructor failures as ordinary runtime data
  - closure evidence:
    - `src/lisp/value_constructors.c3`
      now exposes one checked `make_list2_or_error(...)` helper with a narrow
      nth-failure seam for deterministic runtime/JIT constructor tests.
    - `src/lisp/prim_system.c3`
      now makes `(shell cmd true)` fail closed with
      `"shell: failed to construct result list"` if the final two-item result
      list cannot be built.
    - `src/lisp/jit_jit_runtime_effects_handle.c3`
      now routes both pending-raise and normal effect-handler arg-pair
      construction through the same checked helper, so constructor failure
      propagates before handler call-through.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins the shell result-list construction seam.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now directly pins the pending-raise and signal-handler arg-pair seam in
      the `jit-policy` slice.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=handler-arg-list-alloc-failure ./build/main --test-suite lisp'`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-SCHEDULER-BATCH-RESULT-LIST-FAILCLOSED-030` make scheduler
  batch primitives fail closed when result-list cons construction fails instead
  of publishing partial success or leaking spawned task state
  - closure evidence:
    - `src/lisp/scheduler_primitives_threads.c3`
      now routes batch thread result-list assembly through one checked
      scheduler-local prepend helper with a narrow nth-failure seam.
    - `src/lisp/scheduler_primitives_offload_execute.c3`
      now makes offload batch result-list construction return a typed
      `"offload: out of memory"` error instead of publishing a partial result
      list.
    - `src/lisp/scheduler_primitives_task_spawn.c3`
      now drops already-spawned live thread-task entries if result-list
      publication fails after task creation.
    - `src/lisp/tests_scheduler_groups_more.c3`
      now pins forced result-list cons allocation failure for offload-batch,
      task-spawn-batch, and thread-spawn-batch, and proves active thread-task
      count is unchanged after the failure path.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`

- [x] `AUDIT-ITERATOR-CONS-CONSTRUCTOR-FAILCLOSED-029` make iterator
  coroutine cons construction fail closed instead of publishing constructor
  failures as data or remapping them to misleading apply errors
  - closure evidence:
    - `src/lisp/primitives_iter_coroutine.c3`
      now routes `zip` item-pair and `foldl` arg-list construction through a
      checked iterator-local cons helper with a narrow nth-failure seam.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now proves forced constructor failure makes `zip` return
      `"__iterator-zip: failed to allocate item pair"` and `foldl` return
      `"__iterator-foldl: failed to allocate call args"` instead of embedding
      `ERROR` values into iterator data or degrading to `"arg list too short"`.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-PENDING-RAISE-PAYLOAD-FAILCLOSED-028` make pending raise
  payload/message materialization fail closed instead of binding constructor
  failures as ordinary handler data
  - closure evidence:
    - `src/lisp/value_constructors.c3`
      now rejects `boundary_promote_to_root(...)` null/error results before
      publishing pending raise payload state.
    - `src/lisp/jit_jit_runtime_effects_handle.c3`
      now rejects raise fallback `make_string(...)` and arg-pair
      `make_cons(...)` failure before handler call-through.
    - `src/lisp/jit_jit_handle_signal_handle.c3`
      now rejects raise fallback string materialization failure before clause
      env extension.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now proves the surface `handle` form returns a top-level eval error
      instead of invoking the raise clause when pending raise message
      materialization fails.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now pins the same fail-closed contract in the `jit-policy` slice.
    - validation:
      - `c3c build`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=pending-raise-string-alloc-failure ./build/main --test-suite lisp`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-SCHEDULER-OS-THREAD-NULL-COMPLETION-FAILCLOSED-027` make
  `scheduler_complete_os_thread(...)` drop+wake instead of stranding a running
  OS-thread entry when both completion materialization and alloc-failure
  completion materialization fail
  - closure evidence:
    - `src/lisp/scheduler_thread_task_transition_scaffold.c3`
      now exposes a narrow transition-completion allocation fail seam for
      deterministic scheduler boundary tests.
    - `src/lisp/scheduler_thread_task_transitions.c3`
      now drops the OS-thread entry on null completion plus alloc-failure
      completion OOM instead of returning with the entry still running.
    - `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3`
      now pins the double-failure seam and proves the blocked waiter wakes,
      join token clears, and the OS-thread entry is removed.
    - `src/lisp/tests_scheduler_groups.c3`
      now wires the new boundary regression into the bounded scheduler slice.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-ITERATOR-MALFORMED-TAIL-FAILCLOSED-026` make iterator
  terminal and coroutine helpers reject malformed pair tails instead of
  truncating pipelines as successful completion
  - closure evidence:
    - `src/lisp/primitives_iter_state.c3`
      now exposes one shared `iterator_tail_or_error(...)` helper for
      fail-closed iterator tail validation.
    - `src/lisp/primitives_iter_terminal.c3`
      now makes `collect` / `to-array` reject malformed iterator pairs and
      malformed iterator tails instead of silently truncating the result.
    - `src/lisp/primitives_iter_coroutine.c3`
      now makes `map`, `filter`, `take`, `zip`, and `foldl` reject malformed
      iterator tails instead of truncating or deferring broken state as normal
      completion.
    - `src/lisp/tests_advanced_core_unicode_groups.c3`
      now pins malformed-tail rejection through the surface `List`, `Array`,
      `map`, `filter`, `take`, `zip`, and `foldl` iterator pipelines.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc "rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib ./build/main --eval \"(handle (List (Iterator (lambda () (cons 1 2)))) (raise msg (ref msg 'message)))\""`
      - `scripts/run_validation_container.sh bash -lc "rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib ./build/main --eval \"(handle (foldl (lambda (a x) (+ a x)) 0 (Iterator (lambda () (cons 1 9)))) (raise msg (ref msg 'message)))\""`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-SCHEDULER-WAKEUP-PUBLISH-FALLBACK-025` make scheduler timer,
  sleep, poll-error, and non-task offload wakeups fail closed when reliable
  wakeup enqueue fails instead of dropping the blocked-fiber completion
  - closure evidence:
    - `src/lisp/scheduler_wakeup_callbacks.c3`
      now makes timer, sleep, and poll-error callbacks fall back to the same
      direct wakeup handlers on reliable queue publish failure.
    - `src/lisp/scheduler_offload_worker.c3`
      now makes non-task worker completion fall back to
      `scheduler_handle_wakeup_offload_ready(...)` on publish failure instead
      of freeing the live completion payload.
    - `src/lisp/tests_scheduler_groups_more.c3`
      now pins the real enqueue-failure seam for timer, sleep, poll-error,
      and offload-after fallback.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-SCHEDULER-SHARED-PROJECTION-FAILCLOSED-024` make scheduler
  shared-handle and offload-path projection fail closed instead of publishing
  empty-string or false-success results
  - closure evidence:
    - `src/lisp/scheduler_state_shared_handles.c3`
      now makes `scheduler_project_shared_to_local_value(...)` return
      scheduler `ERROR`s for missing handle refs and shared-payload
      materialization failure instead of an empty string.
    - `src/lisp/scheduler_offload_ops.c3`
      now makes `scheduler_offload_read_file(...)` and
      `scheduler_offload_file_exists(...)` report `OFFLOAD_RES_ERROR` for
      missing/invalid projected path payloads instead of synthesizing
      `nil`/`0` success results.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins both the direct shared-handle projection failure and the
      offload-path projection failure family.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-CONS-ESCAPE-PROMOTION-FAILCLOSED-023` make escape-lane cons
  publication fail closed when string/error promotion cannot actually move the
  field into the ESCAPE lane
  - closure evidence:
    - `src/lisp/value_constructors_core.c3`
      now stages `car` / `cdr` escape promotion before pair allocation,
      rejects null or `ERROR` promotion results, rejects the string/error
      case where promotion falls back to the original non-escape value, and
      unwinds staged promoted fields if final pair allocation fails.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins the forced string/error escape-promotion seam and proves
      `make_cons(...)` returns a typed error instead of publishing an
      ESCAPE-lane cons that still points at a TEMP string.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-DATA-FORMAT-ARRAY-PROMOTION-FAILCLOSED-022` make JSON/TOML
  array assembly reject promoted boundary `ERROR` values instead of storing
  them as ordinary array elements
  - closure evidence:
    - `src/lisp/json.c3`
      now rejects `boundary_promote_to_root(...)` results that come back as
      `ERROR` values during JSON array assembly instead of publishing them
      into successful arrays.
    - `src/lisp/primitives_toml_bridge.c3`
      now applies the same fail-closed rule for TOML array element promotion.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins a TOML array-element promotion fault through the existing
      `TIME_POINT` wrapper-copy allocation seam under a non-root scope and
      proves the array conversion returns the boundary error directly.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-COROUTINE-THUNK-PROMOTION-FAILCLOSED-021` make coroutine thunk
  publication fail closed when root promotion returns an `ERROR` or invalid
  callable state instead of allocating coroutine stack context around a bad
  thunk
  - closure evidence:
    - `src/lisp/primitives_coroutine.c3`
      now makes both `prim_coroutine_prepare_thunk(...)` and
      `prim_coroutine_create_ctx(...)` reject:
      - null promotion results,
      - promoted `ERROR` values,
      - and non-closure / null-closure thunk state
      before any stack context or coroutine wrapper allocation.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins the forced closure-wrapper promotion-allocation seam and proves
      coroutine construction aborts before `stack_ctx_pool` allocation
      counters change.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-APPLY-DISPATCH-FAILCLOSED-020` make malformed primitive and
  partial-application helper paths fail closed instead of dereferencing null
  call targets or silently consuming invalid state
  - closure evidence:
    - `src/lisp/eval_apply.c3`
      now rejects invalid primitive application state when the primitive
      wrapper is null, wrongly tagged, missing `prim_val`, or missing
      `prim_val.func`, and rejects partial-application state when
      `first_arg == null`.
    - `src/lisp/jit_jit_apply_helpers.c3`
      now makes `jit_apply_value_primitive(...)` reject malformed primitive
      wrappers before any function-pointer call-through.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins invalid partial state and invalid primitive-wrapper execution in
      the direct runtime helper lane.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now pins malformed primitive wrapper rejection in the JIT helper lane.
    - validation:
      - `c3c build`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=invalid-primitive-state-fails-closed ./build/main --test-suite lisp`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-APPLY-PROMOTION-FAILCLOSED-019` make partial-application state
  and root-promotion array/hashmap helper surfaces fail closed instead of
  executing impossible state or storing promoted `ERROR` values as ordinary
  data
  - closure evidence:
    - `src/lisp/eval_apply.c3`
      now rejects invalid `PARTIAL_PRIM` state before call-through:
      null/non-partial input, null function pointer, `remaining <= 0`,
      `remaining > 2`, and the inconsistent `remaining == 2` with a prefilled
      `second_arg`.
    - `src/lisp/prim_collection_hashmap.c3`
      now makes `hashmap_set_symbol_checked(...)` and
      `hashmap_set_checked(...)` reject `boundary_promote_to_root(...)`
      results that come back as `ERROR` values instead of inserting them.
    - `src/lisp/prim_io_fs_handles.c3` and
      `src/lisp/primitives_data_formats_csv_parse.c3`
      now make `fs_array_push(...)` and `csv_array_push(...)` reject promoted
      `ERROR` values instead of appending them into successful arrays.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins:
      - invalid partial-application state,
      - checked hashmap insertion under opaque primitive promotion failure,
      - and `fs` / CSV array helper pushes under the same promoted-error seam.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-COLLECTION-ARRAY-FAILCLOSED-018` make collection/apply array
  write helpers fail closed on boundary promotion faults, grow failures, and
  comparator runtime errors
  - closure evidence:
    - `src/lisp/prim_collection_sort_array.c3`
      now makes `sort` / `sort-by` list rebuilding propagate `make_cons(...)`
      failure directly, makes `sort-by` propagate comparator application
      errors instead of silently returning a partial sort, makes `array`,
      `list->array`, `set!` on arrays, and `push!` reject
      `boundary_promote_to_root(...)` failures instead of storing `ERROR`
      values as data, and makes `push!` fail closed on grow allocation
      failure instead of null-dereferencing the new item buffer.
    - `src/lisp/primitives_iter_terminal.c3`
      now makes `collect` propagate list-construction failure directly and
      makes `to-array` reject `boundary_promote_to_root(...)` failures
      instead of returning arrays populated with `ERROR` elements.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins array constructor/mutator and `to-array` boundary-promotion
      failure, `push!` grow failure, and `sort-by` comparator failure.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
