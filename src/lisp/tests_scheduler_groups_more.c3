module lisp;

import std::io;
import std::core::mem;
import main;

fn void scheduler_test_prepare_no_async(void** saved_async) {
    scheduler_init();
    *saved_async = g_scheduler.async_handle;
    g_scheduler.async_handle = null;
    scheduler_reset_wakeup_queue();
}

fn void scheduler_test_restore_no_async(void* saved_async) {
    g_scheduler.async_handle = saved_async;
}

fn void scheduler_test_cleanup_no_async(void* saved_async) {
    scheduler_reset_wakeup_queue();
    scheduler_test_restore_no_async(saved_async);
}

fn void scheduler_test_prepare_single_fiber_no_async(void** saved_async, FiberState state) {
    scheduler_test_prepare_no_async(saved_async);
    g_scheduler.fiber_count = 1;
    g_scheduler.current_fiber_id = NO_FIBER;
    g_scheduler.fibers[0] = {};
    g_scheduler.fibers[0].state = state;
}

fn void scheduler_test_prepare_blocked_pending_read(void** saved_async) {
    scheduler_test_prepare_single_fiber_no_async(saved_async, FIBER_BLOCKED);
    g_scheduler.pending_reads[0] = {};
    g_scheduler.pending_reads[0].active = true;
}

fn void scheduler_test_prepare_blocked_pending_offload(void** saved_async) {
    scheduler_test_prepare_single_fiber_no_async(saved_async, FIBER_BLOCKED);
    g_scheduler.pending_offloads[0] = {};
    g_scheduler.pending_offloads[0].active = true;
}

fn void scheduler_test_cleanup_single_fiber_no_async(void* saved_async) {
    if (g_scheduler.pending_reads[0].buffer != null) {
        mem::free(g_scheduler.pending_reads[0].buffer);
    }
    if (g_scheduler.pending_offloads[0].completion != null) {
        scheduler_free_offload_completion(g_scheduler.pending_offloads[0].completion);
    }
    g_scheduler.pending_reads[0] = {};
    g_scheduler.pending_offloads[0] = {};
    g_scheduler.fibers[0] = {};
    g_scheduler.fiber_count = 0;
    scheduler_reset_wakeup_queue();
    scheduler_test_restore_no_async(saved_async);
}

fn void run_scheduler_wakeup_smoke_test(int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_no_async(&saved_async);

    usz enqueues_before = g_scheduler.reliable_wakeup_enqueues.load();
    bool pushed = scheduler_publish_timer_expired(MAX_FIBERS + 1);
    drain_wakeups();
    usz pending = reliable_wakeup_pending_count();
    usz enqueues_after = g_scheduler.reliable_wakeup_enqueues.load();
    scheduler_test_cleanup_no_async(saved_async);

    if (pushed && pending == 0 && (enqueues_after - enqueues_before) == 1) {
        emit_pass_literal("[PASS] reliable wakeup smoke");
        (*pass)++;
    } else {
        io::printfn("[FAIL] reliable wakeup smoke (pushed=%s pending=%d enqueues=%d)",
            pushed ? "yes" : "no", (long)pending, (long)(enqueues_after - enqueues_before));
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_wraparound_test(int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_no_async(&saved_async);
    usz enqueues_before = g_scheduler.reliable_wakeup_enqueues.load();
    usz failures_before = g_scheduler.wakeup_enqueue_failures.load();

    usz target = 1024;
    usz pushed = 0;
    for (usz i = 0; i < target; i++) {
        if (scheduler_publish_poll_error(MAX_FIBERS + i + 1, -1)) pushed++;
    }
    drain_wakeups();
    usz pending = reliable_wakeup_pending_count();
    usz enqueues_after = g_scheduler.reliable_wakeup_enqueues.load();
    usz failures_after = g_scheduler.wakeup_enqueue_failures.load();
    scheduler_test_cleanup_no_async(saved_async);

    if (pushed == target &&
        pending == 0 &&
        (enqueues_after - enqueues_before) == target &&
        (failures_after - failures_before) == 0) {
        emit_pass_literal("[PASS] reliable wakeup flood");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] reliable wakeup flood (pushed=%d pending=%d enqueues=%d failures=%d)",
            (long)pushed,
            (long)pending,
            (long)(enqueues_after - enqueues_before),
            (long)(failures_after - failures_before)
        );
        (*fail)++;
    }
}

fn void run_scheduler_timer_wakeup_test(int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_blocked_pending_read(&saved_async);

    bool pushed = scheduler_publish_timer_expired(0);
    drain_wakeups();

    bool ok = pushed &&
              g_scheduler.pending_reads[0].timed_out &&
              g_scheduler.pending_reads[0].completed &&
              g_scheduler.fibers[0].state == FIBER_READY;

    scheduler_test_cleanup_single_fiber_no_async(saved_async);

    if (ok) {
        emit_pass_literal("[PASS] timer wakeup transitions in drain");
        (*pass)++;
    } else {
        io::printn("[FAIL] timer wakeup transitions in drain");
        (*fail)++;
    }
}

fn void run_scheduler_offload_wakeup_epoch_guard_test(Interp* interp, int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_blocked_pending_offload(&saved_async);

    g_scheduler.fibers[0].wake_epoch = 7001;
    g_scheduler.pending_offloads[0].wake_epoch = 7001;

    OffloadCompletion* stale = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    OffloadCompletion* fresh = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    bool ok = stale != null && fresh != null;
    if (ok) {
        *stale = {};
        stale.kind = OFFLOAD_RES_INT;
        stale.int_result = 11;
        *fresh = {};
        fresh.kind = OFFLOAD_RES_INT;
        fresh.int_result = 42;
    } else {
        if (stale != null) scheduler_free_offload_completion(stale);
        if (fresh != null) scheduler_free_offload_completion(fresh);
    }

    if (ok) {
        bool pushed_stale = scheduler_publish_offload_ready(0, stale, 7000);
        if (!pushed_stale) scheduler_free_offload_completion(stale);
        drain_wakeups();
        ok = pushed_stale &&
             g_scheduler.pending_offloads[0].active &&
             !g_scheduler.pending_offloads[0].completed &&
             g_scheduler.pending_offloads[0].completion == null &&
             g_scheduler.fibers[0].state == FIBER_BLOCKED;

        bool pushed_fresh = scheduler_publish_offload_ready(0, fresh, 7001);
        if (!pushed_fresh) scheduler_free_offload_completion(fresh);
        drain_wakeups();
        bool delivered = pushed_fresh &&
                         g_scheduler.pending_offloads[0].active &&
                         g_scheduler.pending_offloads[0].completed &&
                         g_scheduler.pending_offloads[0].completion == fresh &&
                         g_scheduler.fibers[0].state == FIBER_READY;
        Value* consumed = delivered ? scheduler_consume_pending_offload(0, interp) : null;
        bool consumed_ok = consumed != null &&
                           consumed.tag == INT &&
                           consumed.int_val == 42;
        bool slot_reset = !g_scheduler.pending_offloads[0].active &&
                          !g_scheduler.pending_offloads[0].completed &&
                          g_scheduler.pending_offloads[0].completion == null;
        ok = ok && delivered && consumed_ok && slot_reset;
    }

    scheduler_test_cleanup_single_fiber_no_async(saved_async);

    if (ok) {
        emit_pass_literal("[PASS] offload wakeup epoch guard drops stale recycled-slot completion");
        (*pass)++;
    } else {
        io::printn("[FAIL] offload wakeup epoch guard drops stale recycled-slot completion");
        (*fail)++;
    }
}

fn void run_scheduler_atomic_cas_semantics_test(Interp* interp, int* pass, int* fail) {
    setup(interp, "(define sched-cas (atomic 7))");
    EvalResult ok_swap = run("(block (atomic-cas! sched-cas 7 9) (atomic-read sched-cas))", interp);
    EvalResult bad_swap = run("(block (atomic-cas! sched-cas 7 11) (atomic-read sched-cas))", interp);
    if (!ok_swap.error.has_error && is_int(ok_swap.value) && ok_swap.value.int_val == 9 &&
        !bad_swap.error.has_error && is_int(bad_swap.value) && bad_swap.value.int_val == 9) {
        emit_pass_literal("[PASS] atomic-cas semantics");
        (*pass)++;
    } else {
        io::printn("[FAIL] atomic-cas semantics");
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_tests(Interp* interp, int* pass, int* fail) {
    run_scheduler_wakeup_smoke_test(pass, fail);
    run_scheduler_wakeup_wraparound_test(pass, fail);
    run_scheduler_timer_wakeup_test(pass, fail);
    run_scheduler_offload_wakeup_epoch_guard_test(interp, pass, fail);
    run_scheduler_atomic_cas_semantics_test(interp, pass, fail);
}
