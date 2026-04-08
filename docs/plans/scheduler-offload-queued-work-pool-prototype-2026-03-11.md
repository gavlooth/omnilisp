# Scheduler Offload Queued-Work Pool Prototype (2026-03-11)

Purpose: close O4.3/O4.4 by prototyping a narrow reuse strategy for offload
request scaffolding and validating boundary/task-handoff behavior.

## Prototype Scope

Only queue scaffolding was pooled:
- `QueuedOffloadWork` envelopes used by `scheduler_enqueue_offload(...)`
- no pooling of `OffloadCompletion` or payload bytes
- no change to cross-boundary ownership of completion payloads

Files:
- `src/lisp/scheduler_state_types.c3`
  - extended `OffloadAdmission` with:
    - `queued_free_head`
    - `queued_free_count`
- `src/lisp/scheduler_offload_worker.c3`
  - added:
    - `scheduler_offload_take_queued_work()`
    - `scheduler_offload_recycle_queued_work(...)`
  - wired recycle/take paths into:
    - `scheduler_enqueue_offload(...)`
    - `scheduler_uv_offload_after_cb(...)`
    - enqueue failure and bind failure paths

## Design Constraints Preserved

- Pool synchronization reuses existing `offload_admission.mu`; no new lock path.
- Pool is bounded by `admission.max_in_flight` to avoid unbounded freelist
  growth.
- Completion/object ownership behavior is unchanged:
  - completion creation and publish semantics unchanged,
  - fallback completion behavior unchanged,
  - `scheduler_publish_offload_ready` cleanup path unchanged.
- Task generation/handoff logic is unchanged:
  - `scheduler_bind_thread_task_request(...)`
  - `scheduler_try_begin_thread_task(...)`
  - `scheduler_complete_thread_task(...)`
  - `scheduler_clear_thread_task_request(...)`

## Validation

Build:

```bash
c3c build
```

Scheduler slice (includes boundary/thread-task/offload reliability tests):

```bash
LD_LIBRARY_PATH=/usr/local/lib \
OMNI_TEST_QUIET=1 \
OMNI_SKIP_TLS_INTEGRATION=1 \
OMNI_LISP_TEST_SLICE=scheduler \
OMNI_SCHEDULER_BENCH=1 \
./build/main
```

Log:
- `build/scheduler_offload_pool_validation_2026-03-11.log`

Observed:
- `=== Unified Tests: 69 passed, 0 failed ===`
- benchmark still valid:

```text
OMNI_BENCH_SUMMARY suite=scheduler_offload_hot_path iters=8192 queue_ms=1 queue_ok=8192 queue_pending=0 queue_enqueue_delta=8192 queue_fail_delta=0 completion_ms=2 completion_ok=8192 completion_alloc_fail=0 http_ms=2 http_ok=8192 http_shared_fail=0 tls_ms=1 tls_ok=8192
```

Conclusion:
- prototype queue-envelope reuse is active,
- generation/task handoff invariants remained intact under scheduler slice,
- offload failure cleanup behavior remained intact under existing boundary tests.
