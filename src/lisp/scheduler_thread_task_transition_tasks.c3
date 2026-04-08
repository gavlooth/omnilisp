module lisp;

fn OffloadCompletion* scheduler_make_task_cancel_completion() {
    return scheduler_make_transition_error_completion("thread task cancelled");
}

fn OffloadCompletion* scheduler_make_task_alloc_failure_completion() {
    return scheduler_make_transition_error_completion("thread task: worker completion alloc failed");
}

fn bool scheduler_try_begin_thread_task(usz task_id, uint generation = 0) {
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) return false;
    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();

    if (!g_scheduler.thread_tasks[task_id].active) return false;
    TaskEntry* task = &g_scheduler.thread_tasks[task_id];
    if (!scheduler_task_generation_matches(task, generation)) return false;
    if (task.state == THREAD_TASK_CANCELLED || task.state == THREAD_TASK_DONE) return false;
    if (task.cancel_requested) {
        usz waiter_fiber_id = task.waiter_fiber_id;
        uint waiter_wake_epoch = task.waiter_wake_epoch;
        task.cancel_requested = false;
        task.request_handle = null;
        scheduler_transition_take_waiter(
            &waiter_fiber_id,
            &waiter_wake_epoch,
            &task.waiter_fiber_id,
            &task.waiter_wake_epoch
        );
        if (task.completion == null) {
            task.completion = scheduler_make_task_cancel_completion();
        }
        if (task.completion == null) {
            // Fail closed: never leave CANCELLED entries without completion payload.
            *task = {};
            scheduler_publish_task_cancel_locked(waiter_fiber_id, waiter_wake_epoch);
            return false;
        }
        task.state = THREAD_TASK_CANCELLED;
        scheduler_publish_task_cancel_locked(waiter_fiber_id, waiter_wake_epoch);
        return false;
    }

    task.state = THREAD_TASK_RUNNING;
    task.request_handle = null;
    return true;
}

fn void scheduler_complete_thread_task(usz task_id, OffloadCompletion* completion, uint generation = 0) {
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) {
        if (completion != null) scheduler_free_offload_completion(completion);
        return;
    }
    if (completion == null) {
        completion = scheduler_make_task_alloc_failure_completion();
        if (completion == null) {
            // Fail closed: never strand a RUNNING task when completion materialization fails.
            scheduler_drop_thread_task(task_id, generation);
            return;
        }
    }

    usz waiter_fiber_id = NO_FIBER;
    uint waiter_wake_epoch = 0;
    g_scheduler.thread_task_mu.lock();
    if (!g_scheduler.thread_tasks[task_id].active) {
        g_scheduler.thread_task_mu.unlock();
        scheduler_free_offload_completion(completion);
        return;
    }
    TaskEntry* task = &g_scheduler.thread_tasks[task_id];
    if (!scheduler_task_generation_matches(task, generation)) {
        g_scheduler.thread_task_mu.unlock();
        scheduler_free_offload_completion(completion);
        return;
    }

    if (task.cancel_requested || task.state == THREAD_TASK_CANCELLED) {
        task.cancel_requested = false;
        task.request_handle = null;
        scheduler_transition_take_waiter(
            &waiter_fiber_id,
            &waiter_wake_epoch,
            &task.waiter_fiber_id,
            &task.waiter_wake_epoch
        );
        if (!scheduler_transition_materialize_cancel_completion(
            &task.completion,
            &completion,
            "thread task cancelled"
        )) {
            // Fail closed on OOM instead of publishing CANCELLED without completion.
            *task = {};
            g_scheduler.thread_task_mu.unlock();
            scheduler_publish_task_cancel_unlocked(waiter_fiber_id, waiter_wake_epoch);
            return;
        }
        task.state = THREAD_TASK_CANCELLED;
        g_scheduler.thread_task_mu.unlock();
        scheduler_publish_task_cancel_unlocked(waiter_fiber_id, waiter_wake_epoch);
        return;
    }

    scheduler_transition_handoff_completion(
        &task.completion,
        completion,
        &task.waiter_fiber_id,
        &task.waiter_wake_epoch,
        &waiter_fiber_id,
        &waiter_wake_epoch
    );
    task.state = THREAD_TASK_DONE;
    task.request_handle = null;
    g_scheduler.thread_task_mu.unlock();
    scheduler_publish_task_completion_unlocked(waiter_fiber_id, waiter_wake_epoch);
}

fn bool scheduler_cancel_thread_task(usz task_id, bool* out_already_done, uint generation = 0) {
    if (out_already_done != null) *out_already_done = false;
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) return false;

    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();
    TaskEntry* task = &g_scheduler.thread_tasks[task_id];
    if (!task.active) return false;
    if (!scheduler_task_generation_matches(task, generation)) return false;

    if (task.state == THREAD_TASK_DONE || task.state == THREAD_TASK_CANCELLED) {
        if (out_already_done != null) *out_already_done = true;
        return true;
    }

    if (task.state == THREAD_TASK_PENDING) {
        usz waiter_fiber_id = task.waiter_fiber_id;
        uint waiter_wake_epoch = task.waiter_wake_epoch;
        if (task.request_handle != null) {
            (void)omni_uv_cancel_req(task.request_handle);
        }
        task.cancel_requested = false;
        task.request_handle = null;
        scheduler_transition_take_waiter(
            &waiter_fiber_id,
            &waiter_wake_epoch,
            &task.waiter_fiber_id,
            &task.waiter_wake_epoch
        );
        if (!scheduler_transition_materialize_cancel_completion(
            &task.completion,
            null,
            "thread task cancelled"
        )) {
            // Fail closed: clear entry instead of entering CANCELLED with null completion.
            *task = {};
            scheduler_publish_task_cancel_locked(waiter_fiber_id, waiter_wake_epoch);
            return true;
        }
        task.state = THREAD_TASK_CANCELLED;
        scheduler_publish_task_cancel_locked(waiter_fiber_id, waiter_wake_epoch);
        return true;
    }

    // Running: cooperative cancel request, completion will convert to cancelled.
    task.cancel_requested = true;
    return true;
}
