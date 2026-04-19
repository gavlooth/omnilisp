module lisp;

import std::io;
import std::core::mem;
import main;

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
