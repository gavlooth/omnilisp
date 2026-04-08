module lisp;


fn int scheduler_register_thread_task_waiter(usz task_id, uint generation, usz waiter_fiber_id) {
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) {
        return SCHEDULER_JOIN_WAIT_INVALID;
    }
    if (waiter_fiber_id >= g_scheduler.fiber_count) return SCHEDULER_JOIN_WAIT_INVALID;

    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();

    TaskEntry* task = &g_scheduler.thread_tasks[task_id];
    if (!task.active || !scheduler_task_generation_matches(task, generation)) {
        return SCHEDULER_JOIN_WAIT_INVALID;
    }
    if (task.state == THREAD_TASK_DONE || task.state == THREAD_TASK_CANCELLED) {
        return SCHEDULER_JOIN_WAIT_READY;
    }
    uint waiter_wake_epoch = scheduler_next_join_wait_token_locked();
    if (waiter_wake_epoch == 0) return SCHEDULER_JOIN_WAIT_INVALID;
    if (task.waiter_fiber_id == NO_FIBER || task.waiter_fiber_id == waiter_fiber_id) {
        task.waiter_fiber_id = waiter_fiber_id;
        task.waiter_wake_epoch = waiter_wake_epoch;
        scheduler_set_join_wait_token_locked(waiter_fiber_id, waiter_wake_epoch);
        return SCHEDULER_JOIN_WAIT_REGISTERED;
    }
    return SCHEDULER_JOIN_WAIT_CONFLICT;
}

fn void scheduler_clear_thread_task_waiter(usz task_id, uint generation, usz waiter_fiber_id) {
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) return;

    g_scheduler.thread_task_mu.lock();
    TaskEntry* task = &g_scheduler.thread_tasks[task_id];
    if (task.active &&
        scheduler_task_generation_matches(task, generation) &&
        task.waiter_fiber_id == waiter_fiber_id) {
        task.waiter_fiber_id = NO_FIBER;
        task.waiter_wake_epoch = 0;
        scheduler_clear_join_wait_token_locked(waiter_fiber_id);
    }
    g_scheduler.thread_task_mu.unlock();
}

fn int scheduler_register_os_thread_waiter(usz thread_id, uint generation, usz waiter_fiber_id) {
    if (thread_id >= MAX_OS_THREADS || !g_scheduler.thread_task_mu_ready) {
        return SCHEDULER_JOIN_WAIT_INVALID;
    }
    if (waiter_fiber_id >= g_scheduler.fiber_count) return SCHEDULER_JOIN_WAIT_INVALID;

    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();

    OsThreadEntry* entry = &g_scheduler.os_threads[thread_id];
    if (!entry.active || !scheduler_os_thread_generation_matches(entry, generation)) {
        return SCHEDULER_JOIN_WAIT_INVALID;
    }
    if (entry.state == OS_THREAD_DONE || entry.state == OS_THREAD_CANCELLED) {
        return SCHEDULER_JOIN_WAIT_READY;
    }
    uint waiter_wake_epoch = scheduler_next_join_wait_token_locked();
    if (waiter_wake_epoch == 0) return SCHEDULER_JOIN_WAIT_INVALID;
    if (entry.waiter_fiber_id == NO_FIBER || entry.waiter_fiber_id == waiter_fiber_id) {
        entry.waiter_fiber_id = waiter_fiber_id;
        entry.waiter_wake_epoch = waiter_wake_epoch;
        scheduler_set_join_wait_token_locked(waiter_fiber_id, waiter_wake_epoch);
        return SCHEDULER_JOIN_WAIT_REGISTERED;
    }
    return SCHEDULER_JOIN_WAIT_CONFLICT;
}

fn void scheduler_clear_os_thread_waiter(usz thread_id, uint generation, usz waiter_fiber_id) {
    if (thread_id >= MAX_OS_THREADS || !g_scheduler.thread_task_mu_ready) return;

    g_scheduler.thread_task_mu.lock();
    OsThreadEntry* entry = &g_scheduler.os_threads[thread_id];
    if (entry.active &&
        scheduler_os_thread_generation_matches(entry, generation) &&
        entry.waiter_fiber_id == waiter_fiber_id) {
        entry.waiter_fiber_id = NO_FIBER;
        entry.waiter_wake_epoch = 0;
        scheduler_clear_join_wait_token_locked(waiter_fiber_id);
    }
    g_scheduler.thread_task_mu.unlock();
}
