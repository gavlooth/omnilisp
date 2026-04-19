module lisp;

import std::core::mem;

int g_scheduler_force_task_join_timeout_timer_start_fail_nth = 0;

fn bool scheduler_force_task_join_timeout_timer_start_fail() {
    if (g_scheduler_force_task_join_timeout_timer_start_fail_nth <= 0) return false;
    g_scheduler_force_task_join_timeout_timer_start_fail_nth--;
    return g_scheduler_force_task_join_timeout_timer_start_fail_nth == 0;
}

fn Value* scheduler_task_join_fiber_impl(
    usz task_id,
    uint generation,
    long timeout_ms,
    SchedulerTaskHandle* handle,
    Interp* interp
) {
    if (timeout_ms < 0) {
        return scheduler_task_join_fiber_until_complete(task_id, generation, handle, interp);
    }

    if (timeout_ms == 0) {
        return scheduler_raise(interp, "scheduler/task-join-timeout", "task-join: timeout");
    }

    usz waiter_id = g_scheduler.current_fiber_id;
    Value* timer_err = scheduler_begin_fiber_join_timeout_timer(
        waiter_id,
        timeout_ms,
        "scheduler/task-join-wait-failed",
        "task-join",
        interp
    );
    if (timer_err != null) return timer_err;
    defer scheduler_close_pending_sleep(waiter_id);

    while (true) {
        OffloadCompletion* completion = scheduler_take_thread_task_completion(task_id, generation);
        if (completion != null) {
            Value* out = scheduler_value_from_offload_completion(completion, interp);
            scheduler_free_offload_completion(completion);
            if (handle != null) {
                handle.task_id = NO_TASK;
                handle.generation = 0;
            }
            return out;
        }

        if (!scheduler_thread_task_exists(task_id, generation)) {
            return scheduler_raise(
                interp,
                "scheduler/task-join-task-already-joined-or-invalid",
                "task-join: task already joined or invalid"
            );
        }

        if (scheduler_fiber_join_timeout_elapsed(waiter_id)) {
            scheduler_clear_thread_task_waiter(task_id, generation, waiter_id);
            return scheduler_raise(interp, "scheduler/task-join-timeout", "task-join: timeout");
        }

        int wait_state = scheduler_register_thread_task_waiter(task_id, generation, waiter_id);
        if (wait_state == SCHEDULER_JOIN_WAIT_READY) continue;
        if (wait_state == SCHEDULER_JOIN_WAIT_INVALID) {
            return scheduler_raise(
                interp,
                "scheduler/task-join-task-already-joined-or-invalid",
                "task-join: task already joined or invalid"
            );
        }
        if (wait_state == SCHEDULER_JOIN_WAIT_CONFLICT) {
            return scheduler_raise(
                interp,
                "scheduler/task-join-already-has-waiter",
                "task-join: task already has a waiting fiber"
            );
        }

        g_scheduler.fibers[waiter_id].state = FIBER_BLOCKED;
        Value*[1] yargs;
        yargs[0] = make_nil(interp);
        Value* y = prim_yield(yargs[..], null, interp);
        if (y != null && y.tag == ERROR) {
            scheduler_clear_thread_task_waiter(task_id, generation, waiter_id);
            scheduler_restore_blocked_fiber_ready(waiter_id);
            return scheduler_wrap_join_wait_error(
                y,
                "scheduler/task-join-wait-failed",
                "task-join: wait failed",
                interp
            );
        }
    }
}

fn Value* scheduler_task_join_impl(
    usz task_id,
    uint generation,
    long timeout_ms,
    SchedulerTaskHandle* handle,
    Interp* interp
) {
    if (scheduler_is_running_fiber_context()) {
        Value* ctx_err = scheduler_require_fiber_root_context("task-join", interp);
        if (ctx_err != null) return ctx_err;
        return scheduler_task_join_fiber_impl(task_id, generation, timeout_ms, handle, interp);
    }

    bool loop_ready = g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null;
    long timeout_deadline_ms = 0;
    void* timeout_timer = null;
    if (timeout_ms > 0 && loop_ready) {
        long now_ms = scheduler_monotonic_ms();
        if (now_ms <= 0) {
            return scheduler_raise(
                interp,
                "scheduler/task-join-wait-failed",
                "task-join: monotonic clock unavailable"
            );
        }
        timeout_deadline_ms = now_ms + timeout_ms;

        usz timer_size = uv_handle_size(UV_HANDLE_TIMER);
        if (timer_size == 0) {
            return scheduler_raise(
                interp,
                "scheduler/task-join-wait-failed",
                "task-join: libuv timer unavailable"
            );
        }
        timeout_timer = mem::malloc(timer_size);
        if (timeout_timer == null) {
            return scheduler_raise(
                interp,
                "scheduler/task-join-wait-failed",
                "task-join: out of memory"
            );
        }
        if (uv_timer_init(g_scheduler.uv_loop, timeout_timer) != 0) {
            mem::free(timeout_timer);
            return scheduler_raise(
                interp,
                "scheduler/task-join-wait-failed",
                "task-join: uv_timer_init failed"
            );
        }
        bool timer_start_failed = scheduler_force_task_join_timeout_timer_start_fail();
        if (!timer_start_failed) {
            timer_start_failed = uv_timer_start(timeout_timer, &scheduler_uv_join_timeout_noop_cb, (ulong)timeout_ms, 0) != 0;
        }
        if (timer_start_failed) {
            uv_close(timeout_timer, &scheduler_free_handle_cb);
            timeout_timer = null;
            return scheduler_raise(
                interp,
                "scheduler/task-join-wait-failed",
                "task-join: uv_timer_start failed"
            );
        }
    }
    defer if (timeout_timer != null) uv_close(timeout_timer, &scheduler_free_handle_cb);

    while (true) {
        OffloadCompletion* completion = scheduler_take_thread_task_completion(task_id, generation);
        if (completion != null) {
            Value* out = scheduler_value_from_offload_completion(completion, interp);
            scheduler_free_offload_completion(completion);
            if (handle != null) {
                handle.task_id = NO_TASK;
                handle.generation = 0;
            }
            return out;
        }

        if (!scheduler_thread_task_exists(task_id, generation)) {
            return scheduler_raise(
                interp,
                "scheduler/task-join-task-already-joined-or-invalid",
                "task-join: task already joined or invalid"
            );
        }

        if (timeout_ms == 0) {
            return scheduler_raise(interp, "scheduler/task-join-timeout", "task-join: timeout");
        }

        if (loop_ready) {
            if (timeout_deadline_ms > 0 && scheduler_remaining_wait_ms(timeout_deadline_ms) == 0) {
                return scheduler_raise(interp, "scheduler/task-join-timeout", "task-join: timeout");
            }
            (void)uv_run(g_scheduler.uv_loop, UV_RUN_ONCE);
            continue;
        }

        int wait_state = scheduler_wait_thread_task_completion_state(task_id, generation, timeout_ms);
        if (wait_state == SCHEDULER_JOIN_WAIT_READY) continue;
        if (wait_state == SCHEDULER_JOIN_WAIT_TIMEOUT) {
            return scheduler_raise(interp, "scheduler/task-join-timeout", "task-join: timeout");
        }
        if (wait_state == SCHEDULER_JOIN_WAIT_INVALID) {
            return scheduler_raise(
                interp,
                "scheduler/task-join-task-already-joined-or-invalid",
                "task-join: task already joined or invalid"
            );
        }
        return scheduler_raise(interp, "scheduler/task-join-wait-failed", "task-join: wait failed");
    }
}
