module lisp;

import std::io;
import std::core::mem;
import main;

fn OffloadCompletion* scheduler_test_custom_offload_callback(OffloadWork work) {
    int* counter = (int*)work.custom_ctx;
    if (counter != null) (*counter)++;

    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) return null;
    *completion = {};
    completion.kind = OFFLOAD_RES_INT;
    completion.int_result = 19;
    return completion;
}

fn OffloadCompletion* scheduler_test_custom_offload_slow_callback(OffloadWork work) {
    int* counter = (int*)work.custom_ctx;
    if (counter != null) (*counter)++;
    c_usleep(25000);

    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) return null;
    *completion = {};
    completion.kind = OFFLOAD_RES_INT;
    completion.int_result = 19;
    return completion;
}

struct SchedulerTestCustomOffloadWorkCtx {
    int* counter;
    long result;
}

fn OffloadCompletion* scheduler_test_custom_offload_work_callback(OffloadWork work) {
    SchedulerTestCustomOffloadWorkCtx* ctx = (SchedulerTestCustomOffloadWorkCtx*)work.custom_ctx;
    if (ctx != null && ctx.counter != null) (*ctx.counter)++;

    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) return null;
    *completion = {};
    completion.kind = OFFLOAD_RES_INT;
    completion.int_result = ctx != null ? ctx.result : 0;
    return completion;
}

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

fn usz scheduler_test_count_active_thread_tasks() {
    usz count = 0;
    for (usz i = 0; i < MAX_THREAD_TASKS; i++) {
        if (g_scheduler.thread_tasks[i].active) count++;
    }
    return count;
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

fn void scheduler_test_prepare_blocked_pending_sleep(void** saved_async) {
    scheduler_test_prepare_single_fiber_no_async(saved_async, FIBER_BLOCKED);
    g_scheduler.pending_sleeps[0] = {};
    g_scheduler.pending_sleeps[0].active = true;
}

fn bool scheduler_test_disable_reliable_wakeup_queue() {
    bool saved = g_scheduler.reliable_wakeups.initialized;
    g_scheduler.reliable_wakeups.initialized = false;
    return saved;
}

fn void scheduler_test_restore_reliable_wakeup_queue(bool saved_initialized) {
    g_scheduler.reliable_wakeups.initialized = saved_initialized;
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
    g_scheduler.pending_sleeps[0] = {};
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

fn void run_scheduler_wakeup_publish_fallback_test(Interp* interp, int* pass, int* fail) {
    bool ok = true;
    char[] failed_phase = "";

    {
        void* saved_async = null;
        scheduler_test_prepare_blocked_pending_read(&saved_async);
        usz failures_before = g_scheduler.wakeup_enqueue_failures.load();
        bool saved_initialized = scheduler_test_disable_reliable_wakeup_queue();

        scheduler_uv_timer_cb(null);

        usz failures_after = g_scheduler.wakeup_enqueue_failures.load();
        bool timer_ok =
            g_scheduler.pending_reads[0].timed_out &&
            g_scheduler.pending_reads[0].completed &&
            g_scheduler.fibers[0].state == FIBER_READY &&
            (failures_after - failures_before) == 1;
        if (!timer_ok && ok) failed_phase = "timer";
        ok = ok && timer_ok;

        scheduler_test_restore_reliable_wakeup_queue(saved_initialized);
        scheduler_test_cleanup_single_fiber_no_async(saved_async);
    }

    {
        void* saved_async = null;
        scheduler_test_prepare_blocked_pending_sleep(&saved_async);
        usz failures_before = g_scheduler.wakeup_enqueue_failures.load();
        bool saved_initialized = scheduler_test_disable_reliable_wakeup_queue();

        scheduler_uv_sleep_timer_cb(null);

        usz failures_after = g_scheduler.wakeup_enqueue_failures.load();
        bool sleep_ok =
            g_scheduler.pending_sleeps[0].completed &&
            g_scheduler.fibers[0].state == FIBER_READY &&
            (failures_after - failures_before) == 1;
        if (!sleep_ok && ok) failed_phase = "sleep";
        ok = ok && sleep_ok;

        scheduler_test_restore_reliable_wakeup_queue(saved_initialized);
        scheduler_test_cleanup_single_fiber_no_async(saved_async);
    }

    {
        void* saved_async = null;
        scheduler_test_prepare_blocked_pending_read(&saved_async);
        usz failures_before = g_scheduler.wakeup_enqueue_failures.load();
        bool saved_initialized = scheduler_test_disable_reliable_wakeup_queue();

        scheduler_uv_poll_cb(null, -17, 0);

        usz failures_after = g_scheduler.wakeup_enqueue_failures.load();
        bool poll_ok =
            g_scheduler.pending_reads[0].error_code == -17 &&
            g_scheduler.pending_reads[0].completed &&
            g_scheduler.fibers[0].state == FIBER_READY &&
            (failures_after - failures_before) == 1;
        if (!poll_ok && ok) failed_phase = "poll";
        ok = ok && poll_ok;

        scheduler_test_restore_reliable_wakeup_queue(saved_initialized);
        scheduler_test_cleanup_single_fiber_no_async(saved_async);
    }

    {
        void* saved_async = null;
        scheduler_test_prepare_blocked_pending_offload(&saved_async);
        usz failures_before = g_scheduler.wakeup_enqueue_failures.load();
        bool saved_initialized = scheduler_test_disable_reliable_wakeup_queue();

        void* req = omni_uv_work_req_new();
        QueuedOffloadWork* queued = scheduler_offload_take_queued_work();
        OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
        bool setup_ok = req != null && queued != null && completion != null;
        if (setup_ok) {
            *completion = {};
            completion.kind = OFFLOAD_RES_INT;
            completion.int_result = 77;
            queued.work = {};
            queued.work.fiber_id = 0;
            queued.work.task_id = NO_TASK;
            queued.completion = completion;
            omni_uv_req_set_data(req, queued);
            scheduler_uv_offload_after_cb(req, 0);
        } else {
            if (completion != null) scheduler_free_offload_completion(completion);
            if (queued != null) scheduler_offload_recycle_queued_work(queued);
            if (req != null) omni_uv_work_req_free(req);
        }

        usz failures_after = g_scheduler.wakeup_enqueue_failures.load();
        bool delivered = setup_ok &&
            g_scheduler.pending_offloads[0].completed &&
            g_scheduler.pending_offloads[0].completion != null &&
            g_scheduler.fibers[0].state == FIBER_READY &&
            (failures_after - failures_before) == 1;
        Value* consumed = delivered ? scheduler_consume_pending_offload(0, interp) : null;
        bool consumed_ok = consumed != null &&
            consumed.tag == INT &&
            consumed.int_val == 77;
        bool slot_reset = !g_scheduler.pending_offloads[0].active &&
            !g_scheduler.pending_offloads[0].completed &&
            g_scheduler.pending_offloads[0].completion == null;
        bool offload_ok = setup_ok && delivered && consumed_ok && slot_reset;
        if (!offload_ok && ok) failed_phase = "offload";
        ok = ok && offload_ok;

        scheduler_test_restore_reliable_wakeup_queue(saved_initialized);
        scheduler_test_cleanup_single_fiber_no_async(saved_async);
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler publish fallback completes blocked wakeups locally");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler publish fallback completes blocked wakeups locally (%s)",
            (ZString)failed_phase);
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
    run_scheduler_wakeup_publish_fallback_test(interp, pass, fail);
    run_scheduler_offload_batch_primitive_tests(interp, pass, fail);
    run_scheduler_task_spawn_batch_primitive_tests(interp, pass, fail);
    run_scheduler_thread_spawn_batch_primitive_tests(interp, pass, fail);
    run_scheduler_batch_result_list_alloc_failure_tests(interp, pass, fail);
    run_scheduler_custom_offload_callback_executor_tests(interp, pass, fail);
    run_scheduler_custom_offload_helper_tests(interp, pass, fail);
    run_scheduler_custom_offload_thread_task_tests(interp, pass, fail);
    run_scheduler_custom_offload_task_helper_tests(interp, pass, fail);
    run_scheduler_custom_offload_task_begin_fanout_tests(interp, pass, fail);
    run_scheduler_custom_offload_task_execute_fanout_tests(interp, pass, fail);
    run_scheduler_custom_offload_work_begin_fanout_tests(interp, pass, fail);
    run_scheduler_custom_offload_work_execute_fanout_tests(interp, pass, fail);
    run_scheduler_custom_os_thread_begin_fanout_tests(interp, pass, fail);
    run_scheduler_custom_os_thread_execute_fanout_tests(interp, pass, fail);
    run_scheduler_custom_os_thread_work_begin_fanout_tests(interp, pass, fail);
    run_scheduler_custom_os_thread_work_execute_fanout_tests(interp, pass, fail);
    run_scheduler_atomic_cas_semantics_test(interp, pass, fail);
}

fn void run_scheduler_offload_batch_primitive_tests(Interp* interp, int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_no_async(&saved_async);
    defer scheduler_test_cleanup_no_async(saved_async);

    EvalResult r = run(
        "(block (define v (__raw-offload-batch '((sleep-ms 0) (sleep-ms 0)))) "
        "       (and (pair? v) "
        "            (pair? (cdr v)) "
        "            (null? (cdr (cdr v))) "
        "            (null? (car v)) "
        "            (null? (car (cdr v)))))",
        interp
    );
    bool ok = !r.error.has_error &&
              is_symbol(r.value) &&
              r.value.sym_val == interp.sym_true;

    if (ok) {
        emit_pass_literal("[PASS] offload batch primitive");
        (*pass)++;
    } else {
        char[256] rendered = {};
        usz len = print_value_to_buf(r.value, &interp.symbols, &rendered, rendered.len);
        if (r.error.has_error) {
            if (len > 0) {
                io::printfn("[FAIL] offload batch primitive (error=%s, value=%s)",
                    (ZString)r.error.message[:256],
                    (ZString)rendered[:len]);
            } else {
                io::printfn("[FAIL] offload batch primitive (error=%s, value=<unprintable>)",
                    (ZString)r.error.message[:256]);
            }
        } else {
            if (len > 0) {
                io::printfn("[FAIL] offload batch primitive (value=%s)", (ZString)rendered[:len]);
            } else {
                io::printfn("[FAIL] offload batch primitive (value=<unprintable>)");
            }
        }
        (*fail)++;
    }
}

fn void run_scheduler_task_spawn_batch_primitive_tests(Interp* interp, int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_no_async(&saved_async);
    defer scheduler_test_cleanup_no_async(saved_async);

    EvalResult r = run(
        "(block (define hs (__raw-task-spawn-batch '((sleep-ms 0) (sleep-ms 0)))) "
        "       (and (pair? hs) "
        "            (pair? (cdr hs)) "
        "            (null? (cdr (cdr hs))) "
        "            (null? (task-join (car hs))) "
        "            (null? (task-join (car (cdr hs))))))",
        interp
    );
    bool ok = !r.error.has_error &&
              is_symbol(r.value) &&
              r.value.sym_val == interp.sym_true;

    if (ok) {
        emit_pass_literal("[PASS] task spawn batch primitive");
        (*pass)++;
    } else {
        char[256] rendered = {};
        usz len = print_value_to_buf(r.value, &interp.symbols, &rendered, rendered.len);
        if (r.error.has_error) {
            if (len > 0) {
                io::printfn("[FAIL] task spawn batch primitive (error=%s, value=%s)",
                    (ZString)r.error.message[:256],
                    (ZString)rendered[:len]);
            } else {
                io::printfn("[FAIL] task spawn batch primitive (error=%s, value=<unprintable>)",
                    (ZString)r.error.message[:256]);
            }
        } else {
            if (len > 0) {
                io::printfn("[FAIL] task spawn batch primitive (value=%s)", (ZString)rendered[:len]);
            } else {
                io::printfn("[FAIL] task spawn batch primitive (value=<unprintable>)");
            }
        }
        (*fail)++;
    }
}

fn void run_scheduler_thread_spawn_batch_primitive_tests(Interp* interp, int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_no_async(&saved_async);
    defer scheduler_test_cleanup_no_async(saved_async);

    EvalResult r = run(
        "(block (define v (__raw-thread-spawn-batch '((sleep-ms 0) (sleep-ms 0)))) "
        "       (and (pair? v) "
        "            (pair? (cdr v)) "
        "            (null? (cdr (cdr v))) "
        "            (null? (car v)) "
        "            (null? (car (cdr v)))))",
        interp
    );
    bool ok = !r.error.has_error &&
              is_symbol(r.value) &&
              r.value.sym_val == interp.sym_true;

    if (ok) {
        emit_pass_literal("[PASS] thread spawn batch primitive");
        (*pass)++;
    } else {
        char[256] rendered = {};
        usz len = print_value_to_buf(r.value, &interp.symbols, &rendered, rendered.len);
        if (r.error.has_error) {
            if (len > 0) {
                io::printfn("[FAIL] thread spawn batch primitive (error=%s, value=%s)",
                    (ZString)r.error.message[:256],
                    (ZString)rendered[:len]);
            } else {
                io::printfn("[FAIL] thread spawn batch primitive (error=%s, value=<unprintable>)",
                    (ZString)r.error.message[:256]);
            }
        } else {
            if (len > 0) {
                io::printfn("[FAIL] thread spawn batch primitive (value=%s)", (ZString)rendered[:len]);
            } else {
                io::printfn("[FAIL] thread spawn batch primitive (value=<unprintable>)");
            }
        }
        (*fail)++;
    }
}

fn void run_scheduler_batch_result_list_alloc_failure_tests(Interp* interp, int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_no_async(&saved_async);
    defer scheduler_test_cleanup_no_async(saved_async);

    usz tasks_before = scheduler_test_count_active_thread_tasks();
    int saved_force = g_scheduler_force_result_list_cons_alloc_fail_nth;
    g_scheduler_force_result_list_cons_alloc_fail_nth = 1;
    EvalResult offload_r = run("(__raw-offload-batch '((sleep-ms 0) (sleep-ms 0)))", interp);
    g_scheduler_force_result_list_cons_alloc_fail_nth = 1;
    EvalResult task_r = run("(__raw-task-spawn-batch '((sleep-ms 0) (sleep-ms 0)))", interp);
    g_scheduler_force_result_list_cons_alloc_fail_nth = 1;
    EvalResult thread_r = run("(__raw-thread-spawn-batch '((sleep-ms 0) (sleep-ms 0)))", interp);
    g_scheduler_force_result_list_cons_alloc_fail_nth = saved_force;
    usz tasks_after = scheduler_test_count_active_thread_tasks();

    bool ok =
        offload_r.error.has_error &&
        str_starts_with(offload_r.error.message[:256], "offload: out of memory") &&
        task_r.error.has_error &&
        str_starts_with(task_r.error.message[:256], "task-spawn-batch: out of memory") &&
        thread_r.error.has_error &&
        str_starts_with(thread_r.error.message[:256], "thread-spawn-batch: out of memory") &&
        tasks_before == tasks_after;

    if (ok) {
        emit_pass_literal("[PASS] scheduler batch result list alloc failures fail closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler batch result list alloc failures fail closed (offload=%s task=%s thread=%s tasks_before=%d tasks_after=%d)",
            offload_r.error.has_error ? (ZString)offload_r.error.message[:256] : (ZString)"<none>",
            task_r.error.has_error ? (ZString)task_r.error.message[:256] : (ZString)"<none>",
            thread_r.error.has_error ? (ZString)thread_r.error.message[:256] : (ZString)"<none>",
            (int)tasks_before,
            (int)tasks_after
        );
        (*fail)++;
    }
}

fn void run_scheduler_custom_offload_callback_executor_tests(Interp* interp, int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_blocked_pending_offload(&saved_async);

    int callback_count = 0;
    OffloadWork work = {};
    work.fiber_id = 0;
    work.task_id = NO_TASK;
    work.op = OFFLOAD_SLEEP_MS;
    work.int_arg = 0;
    work.custom_fn = &scheduler_test_custom_offload_callback;
    work.custom_ctx = &callback_count;

    bool queued = scheduler_enqueue_offload(work);
    bool ready = false;
    OffloadCompletion* completion = null;
    if (queued) {
        for (usz spin = 0; spin < 240; spin++) {
            scheduler_test_pump_wakeups();
            if (g_scheduler.pending_offloads[0].active &&
                g_scheduler.pending_offloads[0].completed &&
                g_scheduler.pending_offloads[0].completion != null &&
                g_scheduler.fibers[0].state == FIBER_READY) {
                ready = true;
                break;
            }
            c_usleep(1000);
        }
        if (ready) {
            completion = (OffloadCompletion*)g_scheduler.pending_offloads[0].completion;
        }
    }

    bool consumed_ok = false;
    if (completion != null) {
        Value* consumed = scheduler_consume_pending_offload(0, interp);
        consumed_ok = consumed != null &&
                      consumed.tag == INT &&
                      consumed.int_val == 19;
    }

    bool ok = queued &&
              ready &&
              callback_count == 1 &&
              consumed_ok &&
              !g_scheduler.pending_offloads[0].active &&
              !g_scheduler.pending_offloads[0].completed &&
              g_scheduler.pending_offloads[0].completion == null;

    scheduler_test_cleanup_single_fiber_no_async(saved_async);

    if (ok) {
        emit_pass_literal("[PASS] custom offload callback executor");
        (*pass)++;
    } else {
        io::printn("[FAIL] custom offload callback executor");
        (*fail)++;
    }
}

fn void run_scheduler_custom_offload_helper_tests(Interp* interp, int* pass, int* fail) {
    int callback_count = 0;
    Value* consumed = scheduler_execute_custom_offload_job(
        &scheduler_test_custom_offload_callback,
        &callback_count,
        interp
    );

    bool ok = consumed != null &&
              consumed.tag == INT &&
              consumed.int_val == 19 &&
              callback_count == 1;

    if (ok) {
        emit_pass_literal("[PASS] custom offload helper executor");
        (*pass)++;
    } else {
        io::printn("[FAIL] custom offload helper executor");
        (*fail)++;
    }
}

fn void run_scheduler_custom_offload_task_helper_tests(Interp* interp, int* pass, int* fail) {
    usz task_id = scheduler_alloc_thread_task();
    uint task_generation = 0;
    if (task_id == NO_TASK ||
        !scheduler_get_thread_task_generation(task_id, &task_generation) ||
        task_generation == 0) {
        if (task_id != NO_TASK) scheduler_drop_thread_task(task_id);
        io::printn("[FAIL] custom offload task helper executor");
        (*fail)++;
        return;
    }

    int callback_count = 0;
    Value* consumed = scheduler_execute_custom_offload_task_job(
        &scheduler_test_custom_offload_callback,
        &callback_count,
        task_id,
        interp
    );

    bool ok = consumed != null &&
              consumed.tag == INT &&
              consumed.int_val == 19 &&
              callback_count == 1 &&
              !scheduler_thread_task_exists(task_id, task_generation);

    if (scheduler_thread_task_exists(task_id, task_generation)) {
        scheduler_drop_thread_task(task_id, task_generation);
    }

    if (ok) {
        emit_pass_literal("[PASS] custom offload task helper executor");
        (*pass)++;
    } else {
        io::printn("[FAIL] custom offload task helper executor");
        (*fail)++;
    }
}

fn void run_scheduler_custom_offload_task_begin_fanout_tests(Interp* interp, int* pass, int* fail) {
    SchedulerTaskHandle first = {};
    SchedulerTaskHandle second = {};
    int first_count = 0;
    int second_count = 0;

    void*[2] ctxs = { &first_count, &second_count };
    SchedulerTaskHandle[2] handles = {};
    Value* begin_err = scheduler_begin_custom_offload_task_jobs(
        &scheduler_test_custom_offload_slow_callback,
        &ctxs[0],
        2,
        &handles[0],
        interp
    );
    if (begin_err == null) {
        first = handles[0];
        second = handles[1];
    }

    bool both_active = begin_err == null &&
                       first.task_id != NO_TASK &&
                       second.task_id != NO_TASK &&
                       scheduler_thread_task_exists(first.task_id, first.generation) &&
                       scheduler_thread_task_exists(second.task_id, second.generation);

    Value* first_join = null;
    Value* second_join = null;
    if (both_active) {
        first_join = scheduler_task_join_impl(first.task_id, first.generation, -1, &first, interp);
        second_join = scheduler_task_join_impl(second.task_id, second.generation, -1, &second, interp);
    }

    bool ok = both_active &&
              first_join != null &&
              second_join != null &&
              first_join.tag == INT &&
              first_join.int_val == 19 &&
              second_join.tag == INT &&
              second_join.int_val == 19 &&
              first_count == 1 &&
              second_count == 1 &&
              !scheduler_thread_task_exists(first.task_id, first.generation) &&
              !scheduler_thread_task_exists(second.task_id, second.generation);

    if (!ok) {
        if (scheduler_thread_task_exists(first.task_id, first.generation)) {
            scheduler_drop_thread_task(first.task_id, first.generation);
        }
        if (scheduler_thread_task_exists(second.task_id, second.generation)) {
            scheduler_drop_thread_task(second.task_id, second.generation);
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] custom offload task begin fanout");
        (*pass)++;
    } else {
        io::printn("[FAIL] custom offload task begin fanout");
        (*fail)++;
    }
}

fn void run_scheduler_custom_offload_task_execute_fanout_tests(Interp* interp, int* pass, int* fail) {
    int first_count = 0;
    int second_count = 0;

    void*[2] ctxs = { &first_count, &second_count };
    Value* consumed = scheduler_execute_custom_offload_task_jobs(
        &scheduler_test_custom_offload_slow_callback,
        &ctxs[0],
        2,
        interp
    );

    bool ok = consumed != null &&
              consumed.tag == VOID &&
              first_count == 1 &&
              second_count == 1;

    if (ok) {
        emit_pass_literal("[PASS] custom offload task execute fanout");
        (*pass)++;
    } else {
        io::printn("[FAIL] custom offload task execute fanout");
        (*fail)++;
    }
}

fn void run_scheduler_custom_offload_work_begin_fanout_tests(Interp* interp, int* pass, int* fail) {
    SchedulerTaskHandle first = {};
    SchedulerTaskHandle second = {};
    int first_count = 0;
    int second_count = 0;

    SchedulerTestCustomOffloadWorkCtx first_ctx = { &first_count, 11 };
    SchedulerTestCustomOffloadWorkCtx second_ctx = { &second_count, 29 };

    OffloadWork[2] works = {};
    works[0].custom_fn = &scheduler_test_custom_offload_work_callback;
    works[0].custom_ctx = &first_ctx;
    works[0].task_id = NO_TASK;
    works[1].custom_fn = &scheduler_test_custom_offload_work_callback;
    works[1].custom_ctx = &second_ctx;
    works[1].task_id = NO_TASK;

    SchedulerTaskHandle[2] handles = {};
    Value* begin_err = scheduler_begin_offload_work_jobs(
        works[..],
        &handles[0],
        interp
    );
    if (begin_err == null) {
        first = handles[0];
        second = handles[1];
    }

    bool both_active = begin_err == null &&
                       first.task_id != NO_TASK &&
                       second.task_id != NO_TASK &&
                       scheduler_thread_task_exists(first.task_id, first.generation) &&
                       scheduler_thread_task_exists(second.task_id, second.generation);

    Value* first_join = null;
    Value* second_join = null;
    if (both_active) {
        first_join = scheduler_task_join_impl(first.task_id, first.generation, -1, &first, interp);
        second_join = scheduler_task_join_impl(second.task_id, second.generation, -1, &second, interp);
    }

    bool ok = both_active &&
              first_join != null &&
              second_join != null &&
              first_join.tag == INT &&
              first_join.int_val == 11 &&
              second_join.tag == INT &&
              second_join.int_val == 29 &&
              first_count == 1 &&
              second_count == 1 &&
              !scheduler_thread_task_exists(first.task_id, first.generation) &&
              !scheduler_thread_task_exists(second.task_id, second.generation);

    if (!ok) {
        if (scheduler_thread_task_exists(first.task_id, first.generation)) {
            scheduler_drop_thread_task(first.task_id, first.generation);
        }
        if (scheduler_thread_task_exists(second.task_id, second.generation)) {
            scheduler_drop_thread_task(second.task_id, second.generation);
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] custom offload work begin fanout");
        (*pass)++;
    } else {
        io::printn("[FAIL] custom offload work begin fanout");
        (*fail)++;
    }
}

fn void run_scheduler_custom_offload_work_execute_fanout_tests(Interp* interp, int* pass, int* fail) {
    int first_count = 0;
    int second_count = 0;

    SchedulerTestCustomOffloadWorkCtx first_ctx = { &first_count, 11 };
    SchedulerTestCustomOffloadWorkCtx second_ctx = { &second_count, 29 };

    OffloadWork[2] works = {};
    works[0].custom_fn = &scheduler_test_custom_offload_work_callback;
    works[0].custom_ctx = &first_ctx;
    works[0].task_id = NO_TASK;
    works[1].custom_fn = &scheduler_test_custom_offload_work_callback;
    works[1].custom_ctx = &second_ctx;
    works[1].task_id = NO_TASK;

    Value* consumed = scheduler_execute_offload_work_jobs(
        works[..],
        interp
    );

    bool ok = consumed != null &&
              consumed.tag == VOID &&
              first_count == 1 &&
              second_count == 1;

    if (ok) {
        emit_pass_literal("[PASS] custom offload work execute fanout");
        (*pass)++;
    } else {
        io::printn("[FAIL] custom offload work execute fanout");
        (*fail)++;
    }
}

fn void run_scheduler_custom_offload_thread_task_tests(Interp* interp, int* pass, int* fail) {
    usz task_id = scheduler_alloc_thread_task();
    uint task_generation = 0;
    if (task_id == NO_TASK ||
        !scheduler_get_thread_task_generation(task_id, &task_generation) ||
        task_generation == 0) {
        if (task_id != NO_TASK) scheduler_drop_thread_task(task_id);
        io::printn("[FAIL] custom offload task executor");
        (*fail)++;
        return;
    }

    int callback_count = 0;
    OffloadWork work = {};
    work.fiber_id = NO_FIBER;
    work.task_id = task_id;
    work.op = OFFLOAD_SLEEP_MS;
    work.int_arg = 0;
    work.shared = null;
    work.custom_fn = &scheduler_test_custom_offload_callback;
    work.custom_ctx = &callback_count;

    bool queued = scheduler_enqueue_offload(work);
    bool ready = false;
    OffloadCompletion* completion = null;
    if (queued) {
        for (usz spin = 0; spin < 240; spin++) {
            scheduler_test_pump_wakeups();
            int wait_state = scheduler_wait_thread_task_completion_state(task_id, task_generation, 0);
            if (wait_state == SCHEDULER_JOIN_WAIT_READY) {
                completion = scheduler_take_thread_task_completion(task_id, task_generation);
                ready = true;
                break;
            }
            c_usleep(1000);
        }
    }

    bool ok = queued &&
              ready &&
              completion != null &&
              completion.kind == OFFLOAD_RES_INT &&
              completion.int_result == 19 &&
              callback_count == 1 &&
              !scheduler_thread_task_exists(task_id, task_generation);

    if (completion != null) scheduler_free_offload_completion(completion);
    if (scheduler_thread_task_exists(task_id, task_generation)) {
        scheduler_drop_thread_task(task_id, task_generation);
    }

    if (ok) {
        emit_pass_literal("[PASS] custom offload task executor");
        (*pass)++;
    } else {
        io::printn("[FAIL] custom offload task executor");
        (*fail)++;
    }
}

fn void run_scheduler_custom_os_thread_begin_fanout_tests(Interp* interp, int* pass, int* fail) {
    SchedulerOsThreadHandle first = {};
    SchedulerOsThreadHandle second = {};
    int first_count = 0;
    int second_count = 0;

    void*[2] ctxs = { &first_count, &second_count };
    SchedulerOsThreadHandle[2] handles = {};
    Value* begin_err = scheduler_begin_custom_os_thread_jobs(
        &scheduler_test_custom_offload_slow_callback,
        &ctxs[0],
        2,
        &handles[0],
        interp
    );
    if (begin_err == null) {
        first = handles[0];
        second = handles[1];
    }

    bool both_active = begin_err == null &&
                       first.thread_id != NO_OS_THREAD &&
                       second.thread_id != NO_OS_THREAD &&
                       scheduler_os_thread_exists(first.thread_id, first.generation) &&
                       scheduler_os_thread_exists(second.thread_id, second.generation);

    Value* first_join = null;
    Value* second_join = null;
    if (both_active) {
        first_join = scheduler_thread_join_impl(first.thread_id, first.generation, -1, &first, interp);
        second_join = scheduler_thread_join_impl(second.thread_id, second.generation, -1, &second, interp);
    }

    bool ok = both_active &&
              first_join != null &&
              second_join != null &&
              first_join.tag == INT &&
              first_join.int_val == 19 &&
              second_join.tag == INT &&
              second_join.int_val == 19 &&
              first_count == 1 &&
              second_count == 1 &&
              !scheduler_os_thread_exists(first.thread_id, first.generation) &&
              !scheduler_os_thread_exists(second.thread_id, second.generation);

    if (!ok) {
        if (scheduler_os_thread_exists(first.thread_id, first.generation)) {
            scheduler_drop_os_thread(first.thread_id, first.generation);
        }
        if (scheduler_os_thread_exists(second.thread_id, second.generation)) {
            scheduler_drop_os_thread(second.thread_id, second.generation);
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] custom os thread begin fanout");
        (*pass)++;
    } else {
        io::printn("[FAIL] custom os thread begin fanout");
        (*fail)++;
    }
}

fn void run_scheduler_custom_os_thread_execute_fanout_tests(Interp* interp, int* pass, int* fail) {
    int first_count = 0;
    int second_count = 0;

    void*[2] ctxs = { &first_count, &second_count };
    Value* consumed = scheduler_execute_custom_os_thread_jobs(
        &scheduler_test_custom_offload_slow_callback,
        &ctxs[0],
        2,
        interp
    );

    bool ok = consumed != null &&
              consumed.tag == VOID &&
              first_count == 1 &&
              second_count == 1;

    if (ok) {
        emit_pass_literal("[PASS] custom os thread execute fanout");
        (*pass)++;
    } else {
        io::printn("[FAIL] custom os thread execute fanout");
        (*fail)++;
    }
}

fn void run_scheduler_custom_os_thread_work_begin_fanout_tests(Interp* interp, int* pass, int* fail) {
    int first_count = 0;
    int second_count = 0;

    SchedulerTestCustomOffloadWorkCtx first_ctx = { &first_count, 11 };
    SchedulerTestCustomOffloadWorkCtx second_ctx = { &second_count, 29 };

    OffloadWork[2] works = {};
    works[0].custom_fn = &scheduler_test_custom_offload_work_callback;
    works[0].custom_ctx = &first_ctx;
    works[1].custom_fn = &scheduler_test_custom_offload_work_callback;
    works[1].custom_ctx = &second_ctx;

    SchedulerOsThreadHandle[2] handles = {};
    Value* begin_err = scheduler_begin_os_thread_work_jobs(
        works[..],
        &handles[0],
        interp
    );
    bool ok = begin_err == null &&
              handles[0].thread_id != NO_OS_THREAD &&
              handles[1].thread_id != NO_OS_THREAD;
    Value* first_join = null;
    Value* second_join = null;
    if (ok) {
        first_join = scheduler_thread_join_impl(handles[0].thread_id, handles[0].generation, -1, &handles[0], interp);
        second_join = scheduler_thread_join_impl(handles[1].thread_id, handles[1].generation, -1, &handles[1], interp);
        ok = first_join != null &&
             second_join != null &&
             first_join.tag == INT &&
             first_join.int_val == 11 &&
             second_join.tag == INT &&
             second_join.int_val == 29 &&
             first_count == 1 &&
             second_count == 1;
    }

    if (!ok) {
        if (scheduler_os_thread_exists(handles[0].thread_id, handles[0].generation)) {
            scheduler_drop_os_thread(handles[0].thread_id, handles[0].generation);
        }
        if (scheduler_os_thread_exists(handles[1].thread_id, handles[1].generation)) {
            scheduler_drop_os_thread(handles[1].thread_id, handles[1].generation);
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] custom os thread work begin fanout");
        (*pass)++;
    } else {
        io::printn("[FAIL] custom os thread work begin fanout");
        (*fail)++;
    }
}

fn void run_scheduler_custom_os_thread_work_execute_fanout_tests(Interp* interp, int* pass, int* fail) {
    int first_count = 0;
    int second_count = 0;

    SchedulerTestCustomOffloadWorkCtx first_ctx = { &first_count, 11 };
    SchedulerTestCustomOffloadWorkCtx second_ctx = { &second_count, 29 };

    OffloadWork[2] works = {};
    works[0].custom_fn = &scheduler_test_custom_offload_work_callback;
    works[0].custom_ctx = &first_ctx;
    works[1].custom_fn = &scheduler_test_custom_offload_work_callback;
    works[1].custom_ctx = &second_ctx;

    Value* consumed = scheduler_execute_os_thread_work_jobs(
        works[..],
        interp
    );

    bool ok = consumed != null &&
              consumed.tag == VOID &&
              first_count == 1 &&
              second_count == 1;

    if (ok) {
        emit_pass_literal("[PASS] custom os thread work execute fanout");
        (*pass)++;
    } else {
        io::printn("[FAIL] custom os thread work execute fanout");
        (*fail)++;
    }
}
