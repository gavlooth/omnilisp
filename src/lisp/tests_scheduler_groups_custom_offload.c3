module lisp;

import std::io;
import std::core::mem;
import main;

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
