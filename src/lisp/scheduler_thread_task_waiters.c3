module lisp;

import std::thread;

fn long scheduler_monotonic_ms() @inline {
    long[2] ts;
    if (c_clock_gettime(SCHEDULER_CLOCK_MONOTONIC, &ts) != 0) return 0;
    return ts[0] * 1000 + ts[1] / 1000000;
}

fn long scheduler_remaining_wait_ms(long deadline_ms) @inline {
    long now_ms = scheduler_monotonic_ms();
    if (now_ms <= 0) return 0;
    long remaining_ms = deadline_ms - now_ms;
    return remaining_ms > 0 ? remaining_ms : 0;
}

fn uint scheduler_next_join_wait_token_locked() @inline {
    g_scheduler.join_wait_token_seed++;
    if (g_scheduler.join_wait_token_seed == 0) g_scheduler.join_wait_token_seed = 1;
    return g_scheduler.join_wait_token_seed;
}

fn void scheduler_set_join_wait_token_locked(usz waiter_fiber_id, uint token) @inline {
    if (waiter_fiber_id >= MAX_FIBERS) return;
    g_scheduler.fiber_join_wait_token[waiter_fiber_id] = token;
}

fn void scheduler_clear_join_wait_token_locked(usz waiter_fiber_id) @inline {
    scheduler_set_join_wait_token_locked(waiter_fiber_id, 0);
}

fn void scheduler_signal_thread_task_waiters_locked() @inline {
    if (!g_scheduler.thread_task_cv_ready) return;
    (void)g_scheduler.thread_task_cv.broadcast();
}

fn void scheduler_signal_thread_task_waiters() @inline {
    if (!g_scheduler.thread_task_mu_ready || !g_scheduler.thread_task_cv_ready) return;
    g_scheduler.thread_task_mu.lock();
    scheduler_signal_thread_task_waiters_locked();
    g_scheduler.thread_task_mu.unlock();
}

fn bool scheduler_wait_thread_task_waiters(long timeout_ms) {
    if (!g_scheduler.thread_task_mu_ready || !g_scheduler.thread_task_cv_ready) return false;

    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();

    if (timeout_ms < 0) {
        (void)g_scheduler.thread_task_cv.wait(&g_scheduler.thread_task_mu);
        return true;
    }
    if (timeout_ms == 0) return true;

    if (catch err = g_scheduler.thread_task_cv.wait_timeout(&g_scheduler.thread_task_mu, (ulong)timeout_ms)) {
        // WAIT_TIMEOUT is expected; caller should continue timeout budgeting.
        if (err == thread::WAIT_TIMEOUT) return true;
        return false;
    }
    return true;
}

fn int scheduler_wait_thread_task_completion_state(usz task_id, uint generation, long timeout_ms) {
    if (task_id >= MAX_THREAD_TASKS ||
        !g_scheduler.thread_task_mu_ready ||
        !g_scheduler.thread_task_cv_ready) {
        return SCHEDULER_JOIN_WAIT_INVALID;
    }

    bool finite_timeout = timeout_ms > 0;
    long deadline_ms = 0;
    if (finite_timeout) {
        long now_ms = scheduler_monotonic_ms();
        if (now_ms <= 0) return SCHEDULER_JOIN_WAIT_TIMEOUT;
        deadline_ms = now_ms + timeout_ms;
    }

    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();

    while (true) {
        TaskEntry* task = &g_scheduler.thread_tasks[task_id];
        if (!task.active || !scheduler_task_generation_matches(task, generation)) {
            return SCHEDULER_JOIN_WAIT_INVALID;
        }
        if (task.state == THREAD_TASK_DONE || task.state == THREAD_TASK_CANCELLED) {
            return SCHEDULER_JOIN_WAIT_READY;
        }

        if (timeout_ms == 0) return SCHEDULER_JOIN_WAIT_TIMEOUT;
        if (timeout_ms < 0) {
            (void)g_scheduler.thread_task_cv.wait(&g_scheduler.thread_task_mu);
            continue;
        }

        long wait_ms = timeout_ms;
        if (finite_timeout) {
            wait_ms = scheduler_remaining_wait_ms(deadline_ms);
            if (wait_ms <= 0) return SCHEDULER_JOIN_WAIT_TIMEOUT;
        }

        if (catch err = g_scheduler.thread_task_cv.wait_timeout(&g_scheduler.thread_task_mu, (ulong)wait_ms)) {
            if (err == thread::WAIT_TIMEOUT) return SCHEDULER_JOIN_WAIT_TIMEOUT;
            return SCHEDULER_JOIN_WAIT_FAILED;
        }

        if (finite_timeout) {
            long remaining_ms = scheduler_remaining_wait_ms(deadline_ms);
            timeout_ms = remaining_ms;
        }
    }
}

fn int scheduler_wait_os_thread_completion_state(usz thread_id, uint generation, long timeout_ms) {
    if (thread_id >= MAX_OS_THREADS ||
        !g_scheduler.thread_task_mu_ready ||
        !g_scheduler.thread_task_cv_ready) {
        return SCHEDULER_JOIN_WAIT_INVALID;
    }

    bool finite_timeout = timeout_ms > 0;
    long deadline_ms = 0;
    if (finite_timeout) {
        long now_ms = scheduler_monotonic_ms();
        if (now_ms <= 0) return SCHEDULER_JOIN_WAIT_TIMEOUT;
        deadline_ms = now_ms + timeout_ms;
    }

    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();

    while (true) {
        OsThreadEntry* entry = &g_scheduler.os_threads[thread_id];
        if (!entry.active || !scheduler_os_thread_generation_matches(entry, generation)) {
            return SCHEDULER_JOIN_WAIT_INVALID;
        }
        if (entry.state == OS_THREAD_DONE || entry.state == OS_THREAD_CANCELLED) {
            return SCHEDULER_JOIN_WAIT_READY;
        }

        if (timeout_ms == 0) return SCHEDULER_JOIN_WAIT_TIMEOUT;
        if (timeout_ms < 0) {
            (void)g_scheduler.thread_task_cv.wait(&g_scheduler.thread_task_mu);
            continue;
        }

        long wait_ms = timeout_ms;
        if (finite_timeout) {
            wait_ms = scheduler_remaining_wait_ms(deadline_ms);
            if (wait_ms <= 0) return SCHEDULER_JOIN_WAIT_TIMEOUT;
        }

        if (catch err = g_scheduler.thread_task_cv.wait_timeout(&g_scheduler.thread_task_mu, (ulong)wait_ms)) {
            if (err == thread::WAIT_TIMEOUT) return SCHEDULER_JOIN_WAIT_TIMEOUT;
            return SCHEDULER_JOIN_WAIT_FAILED;
        }

        if (finite_timeout) {
            long remaining_ms = scheduler_remaining_wait_ms(deadline_ms);
            timeout_ms = remaining_ms;
        }
    }
}

fn bool scheduler_latch_join_wakeup_epoch(
    usz waiter_fiber_id,
    uint waiter_wake_epoch,
    bool os_thread_done,
    bool thread_task_mu_locked = false
) {
    if (waiter_fiber_id == NO_FIBER || waiter_wake_epoch == 0) return false;
    if (waiter_fiber_id >= MAX_FIBERS) return false;
    if (!g_scheduler.thread_task_mu_ready) return false;

    if (!thread_task_mu_locked) g_scheduler.thread_task_mu.lock();
    bool latched = false;
    if (waiter_fiber_id < g_scheduler.fiber_count) {
        if (os_thread_done) {
            g_scheduler.latched_os_thread_done_wake_epoch[waiter_fiber_id] = waiter_wake_epoch;
        } else {
            g_scheduler.latched_task_done_wake_epoch[waiter_fiber_id] = waiter_wake_epoch;
        }
        latched = true;
    }
    if (!thread_task_mu_locked) g_scheduler.thread_task_mu.unlock();

    if (latched) scheduler_signal_async();
    return latched;
}

fn bool scheduler_signal_task_waiter_fiber(
    usz waiter_fiber_id,
    uint waiter_wake_epoch,
    bool thread_task_mu_locked = false
) {
    if (waiter_fiber_id == NO_FIBER || waiter_wake_epoch == 0) return true;
    if (scheduler_publish_task_done(waiter_fiber_id, waiter_wake_epoch)) return true;
    return scheduler_latch_join_wakeup_epoch(waiter_fiber_id, waiter_wake_epoch, false, thread_task_mu_locked);
}

fn bool scheduler_signal_os_thread_waiter_fiber(
    usz waiter_fiber_id,
    uint waiter_wake_epoch,
    bool thread_task_mu_locked = false
) {
    if (waiter_fiber_id == NO_FIBER || waiter_wake_epoch == 0) return true;
    if (scheduler_publish_os_thread_done(waiter_fiber_id, waiter_wake_epoch)) return true;
    return scheduler_latch_join_wakeup_epoch(waiter_fiber_id, waiter_wake_epoch, true, thread_task_mu_locked);
}

fn void scheduler_clear_thread_task_waiters_for_fiber(usz waiter_fiber_id) {
    if (waiter_fiber_id == NO_FIBER || !g_scheduler.thread_task_mu_ready) return;
    g_scheduler.thread_task_mu.lock();
    for (usz i = 0; i < MAX_THREAD_TASKS; i++) {
        if (!g_scheduler.thread_tasks[i].active) continue;
        if (g_scheduler.thread_tasks[i].waiter_fiber_id == waiter_fiber_id) {
            g_scheduler.thread_tasks[i].waiter_fiber_id = NO_FIBER;
            g_scheduler.thread_tasks[i].waiter_wake_epoch = 0;
        }
    }
    for (usz i = 0; i < MAX_OS_THREADS; i++) {
        if (!g_scheduler.os_threads[i].active) continue;
        if (g_scheduler.os_threads[i].waiter_fiber_id == waiter_fiber_id) {
            g_scheduler.os_threads[i].waiter_fiber_id = NO_FIBER;
            g_scheduler.os_threads[i].waiter_wake_epoch = 0;
        }
    }
    scheduler_clear_join_wait_token_locked(waiter_fiber_id);
    g_scheduler.latched_task_done_wake_epoch[waiter_fiber_id] = 0;
    g_scheduler.latched_os_thread_done_wake_epoch[waiter_fiber_id] = 0;
    g_scheduler.thread_task_mu.unlock();
}


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
