# Scheduler/Offload Hot-Path Benchmark Baseline (2026-03-11)

Purpose: close O4.1 in `TODO.md` by adding explicit micro-bench coverage for
scheduler + async I/O/offload interaction hotspots and recording the baseline.

## Benchmark Surface

Implemented in:
- `src/lisp/tests_scheduler_io_task_groups.c3`
  - `run_scheduler_offload_hot_path_benchmarks(...)`
  - gate: `OMNI_SCHEDULER_BENCH=1`
  - summary line: `OMNI_BENCH_SUMMARY suite=scheduler_offload_hot_path ...`

Workload:
- `iters=8192` per lane
- queue lane:
  - reliable wakeup publish (`scheduler_publish_poll_error`)
  - queue drain (`drain_wakeups`)
- completion lane:
  - offload-ready publish (`scheduler_publish_offload_ready`)
  - completion consume (`scheduler_consume_pending_offload`)
- HTTP offload lane:
  - worker dispatch fast-fail path (`OFFLOAD_HTTP_GET` with invalid URL)
- TLS offload lane:
  - worker dispatch fast-fail path (`OFFLOAD_TLS_CONNECT` with invalid handle)

## Baseline Run

Command:

```bash
LD_LIBRARY_PATH=/usr/local/lib \
OMNI_TEST_QUIET=1 \
OMNI_SKIP_TLS_INTEGRATION=1 \
OMNI_LISP_TEST_SLICE=scheduler \
OMNI_SCHEDULER_BENCH=1 \
./build/main
```

Log artifact:
- `build/scheduler_offload_bench_2026-03-11.log`

Captured summary:

```text
OMNI_BENCH_SUMMARY suite=scheduler_offload_hot_path iters=8192 queue_ms=1 queue_ok=8192 queue_pending=0 queue_enqueue_delta=8192 queue_fail_delta=0 completion_ms=2 completion_ok=8192 completion_alloc_fail=0 http_ms=2 http_ok=8192 http_shared_fail=0 tls_ms=1 tls_ok=8192
```

## Notes

- All lanes validated full-pass correctness (`*_ok=8192`).
- This benchmark is env-gated and does not run in default scheduler tests.
- The HTTP/TLS lanes intentionally use deterministic fast-fail fixtures to keep
  timing stable while still exercising the offload worker dispatch surface.
