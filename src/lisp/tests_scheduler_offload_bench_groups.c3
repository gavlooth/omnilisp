module lisp;

import std::core::mem;
import std::io;

const usz SCHEDULER_OFFLOAD_BENCH_ITERS = 8192;

fn bool scheduler_offload_bench_enabled() @inline {
    return test_c_getenv("OMNI_SCHEDULER_BENCH") != null;
}

fn long scheduler_offload_bench_now_ms() @inline {
    long now_ms = scheduler_monotonic_ms();
    if (now_ms > 0) return now_ms;
    return 0;
}

fn void run_scheduler_offload_hot_path_benchmarks(Interp* interp) {
    if (!scheduler_offload_bench_enabled()) return;
    if (interp == null) return;

    emit_test_section_banner("Scheduler/Offload Hot-Path Benchmarks");

    scheduler_offload_queued_work_free_hits = 0;
    scheduler_offload_queued_work_alloc_fallbacks = 0;
    scheduler_offload_queued_work_recycle_retained = 0;
    scheduler_offload_queued_work_recycle_freed = 0;
    scheduler_offload_completion_alloc_fallbacks = 0;

    long queue_ms = 0;
    long completion_ms = 0;
    long http_ms = 0;
    long tls_ms = 0;

    usz queue_ok = 0;
    usz completion_ok = 0;
    usz completion_alloc_fail = 0;
    usz http_ok = 0;
    usz http_shared_fail = 0;
    usz tls_ok = 0;

    usz queue_pending = 0;
    usz queue_enqueue_delta = 0;
    usz queue_fail_delta = 0;

    // Reliable wakeup queue publish + drain hot path.
    {
        void* saved_async = null;
        scheduler_test_prepare_no_async(&saved_async);
        usz enqueues_before = g_scheduler.reliable_wakeup_enqueues.load();
        usz failures_before = g_scheduler.wakeup_enqueue_failures.load();

        long start_ms = scheduler_offload_bench_now_ms();
        for (usz i = 0; i < SCHEDULER_OFFLOAD_BENCH_ITERS; i++) {
            if (scheduler_publish_poll_error(MAX_FIBERS + 6000 + i, -1)) queue_ok++;
        }
        drain_wakeups();
        long end_ms = scheduler_offload_bench_now_ms();

        queue_pending = reliable_wakeup_pending_count();
        queue_enqueue_delta = g_scheduler.reliable_wakeup_enqueues.load() - enqueues_before;
        queue_fail_delta = g_scheduler.wakeup_enqueue_failures.load() - failures_before;
        queue_ms = end_ms > start_ms ? end_ms - start_ms : 0;

        scheduler_test_cleanup_no_async(saved_async);
    }

    // Completion publish + consume hot path.
    {
        void* saved_async = null;
        scheduler_test_prepare_single_fiber_no_async(&saved_async, FIBER_BLOCKED);

        long start_ms = scheduler_offload_bench_now_ms();
        for (usz i = 0; i < SCHEDULER_OFFLOAD_BENCH_ITERS; i++) {
            g_scheduler.pending_offloads[0] = {};
            g_scheduler.pending_offloads[0].active = true;
            g_scheduler.fibers[0].state = FIBER_BLOCKED;

            OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
            if (completion == null) {
                completion_alloc_fail++;
                continue;
            }
            *completion = {};
            completion.kind = OFFLOAD_RES_NIL;

            bool pushed = scheduler_publish_offload_ready(0, completion);
            if (!pushed) {
                scheduler_free_offload_completion(completion);
                continue;
            }

            drain_wakeups();
            Value* consumed = scheduler_consume_pending_offload(0, interp);
            bool ok = consumed != null &&
                consumed.tag == NIL &&
                !g_scheduler.pending_offloads[0].active &&
                !g_scheduler.pending_offloads[0].completed &&
                g_scheduler.pending_offloads[0].completion == null;
            if (ok) completion_ok++;
        }
        long end_ms = scheduler_offload_bench_now_ms();
        completion_ms = end_ms > start_ms ? end_ms - start_ms : 0;

        scheduler_test_cleanup_single_fiber_no_async(saved_async);
    }

    // Fast-fail HTTP offload worker dispatch path.
    {
        char[] invalid_url = "invalid-url";
        long start_ms = scheduler_offload_bench_now_ms();
        for (usz i = 0; i < SCHEDULER_OFFLOAD_BENCH_ITERS; i++) {
            OffloadWork work = {};
            work.fiber_id = NO_FIBER;
            work.task_id = NO_TASK;
            work.op = OFFLOAD_HTTP_GET;
            work.shared = scheduler_domain_publish_shared(scheduler_primary_domain(), invalid_url);
            if (work.shared == null) {
                http_shared_fail++;
                continue;
            }

            OffloadCompletion* completion = scheduler_execute_offload(work);
            if (completion != null && completion.kind == OFFLOAD_RES_ERROR) {
                http_ok++;
            }
            if (completion != null) scheduler_free_offload_completion(completion);
        }
        long end_ms = scheduler_offload_bench_now_ms();
        http_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Fast-fail TLS offload worker dispatch path.
    {
        long start_ms = scheduler_offload_bench_now_ms();
        for (usz i = 0; i < SCHEDULER_OFFLOAD_BENCH_ITERS; i++) {
            OffloadWork work = {};
            work.fiber_id = NO_FIBER;
            work.task_id = NO_TASK;
            work.op = OFFLOAD_TLS_CONNECT;
            work.int_arg = 0;

            OffloadCompletion* completion = scheduler_execute_offload(work);
            if (completion != null && completion.kind == OFFLOAD_RES_ERROR) tls_ok++;
            if (completion != null) scheduler_free_offload_completion(completion);
        }
        long end_ms = scheduler_offload_bench_now_ms();
        tls_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    io::printfn(
        "OMNI_BENCH_SUMMARY suite=scheduler_offload_hot_path iters=%d queue_ms=%d queue_ok=%d queue_pending=%d queue_enqueue_delta=%d queue_fail_delta=%d queue_free_hits=%d queue_alloc_fallbacks=%d queue_recycle_retained=%d queue_recycle_freed=%d completion_alloc_fallbacks=%d completion_ms=%d completion_ok=%d completion_alloc_fail=%d http_ms=%d http_ok=%d http_shared_fail=%d tls_ms=%d tls_ok=%d",
        (long)SCHEDULER_OFFLOAD_BENCH_ITERS,
        queue_ms,
        (long)queue_ok,
        (long)queue_pending,
        (long)queue_enqueue_delta,
        (long)queue_fail_delta,
        (long)scheduler_offload_queued_work_free_hits,
        (long)scheduler_offload_queued_work_alloc_fallbacks,
        (long)scheduler_offload_queued_work_recycle_retained,
        (long)scheduler_offload_queued_work_recycle_freed,
        (long)scheduler_offload_completion_alloc_fallbacks,
        completion_ms,
        (long)completion_ok,
        (long)completion_alloc_fail,
        http_ms,
        (long)http_ok,
        (long)http_shared_fail,
        tls_ms,
        (long)tls_ok
    );
}
