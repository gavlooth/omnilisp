module lisp;

fn OffloadCompletion* scheduler_make_task_cancel_completion() {
    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) return null;
    *completion = {};
    scheduler_set_offload_error(completion, "thread task cancelled");
    return completion;
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
        task.cancel_requested = false;
        task.state = THREAD_TASK_CANCELLED;
        task.request_handle = null;
        task.waiter_fiber_id = NO_FIBER;
        if (task.completion == null) {
            task.completion = scheduler_make_task_cancel_completion();
        }
        scheduler_signal_thread_task_waiters_locked();
        scheduler_signal_task_waiter_fiber(waiter_fiber_id);
        return false;
    }

    task.state = THREAD_TASK_RUNNING;
    task.request_handle = null;
    return true;
}

fn void scheduler_complete_thread_task(usz task_id, OffloadCompletion* completion, uint generation = 0) {
    if (completion == null) return;
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) {
        scheduler_free_offload_completion(completion);
        return;
    }

    usz waiter_fiber_id = NO_FIBER;
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
        waiter_fiber_id = task.waiter_fiber_id;
        task.waiter_fiber_id = NO_FIBER;
        scheduler_free_offload_completion(completion);
        if (task.completion == null) {
            task.completion = scheduler_make_task_cancel_completion();
        }
        task.state = THREAD_TASK_CANCELLED;
        g_scheduler.thread_task_mu.unlock();
        scheduler_signal_thread_task_waiters();
        scheduler_signal_task_waiter_fiber(waiter_fiber_id);
        return;
    }

    if (task.completion != null) scheduler_free_offload_completion(task.completion);
    task.completion = completion;
    task.state = THREAD_TASK_DONE;
    task.request_handle = null;
    waiter_fiber_id = task.waiter_fiber_id;
    task.waiter_fiber_id = NO_FIBER;
    g_scheduler.thread_task_mu.unlock();
    scheduler_signal_thread_task_waiters();
    scheduler_signal_task_waiter_fiber(waiter_fiber_id);
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
        if (task.request_handle != null) {
            (void)omni_uv_cancel_req(task.request_handle);
        }
        task.state = THREAD_TASK_CANCELLED;
        task.cancel_requested = false;
        task.request_handle = null;
        task.waiter_fiber_id = NO_FIBER;
        if (task.completion == null) {
            task.completion = scheduler_make_task_cancel_completion();
        }
        scheduler_signal_thread_task_waiters_locked();
        scheduler_signal_task_waiter_fiber(waiter_fiber_id);
        return true;
    }

    // Running: cooperative cancel request, completion will convert to cancelled.
    task.cancel_requested = true;
    return true;
}

fn OffloadCompletion* scheduler_make_os_thread_cancel_completion() {
    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) return null;
    *completion = {};
    scheduler_set_offload_error(completion, "thread cancelled");
    return completion;
}

fn OffloadCompletion* scheduler_make_os_thread_alloc_failure_completion() {
    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) return null;
    *completion = {};
    scheduler_set_offload_error(completion, "thread: worker completion alloc failed");
    return completion;
}

fn bool scheduler_try_begin_os_thread(usz thread_id, uint generation) {
    if (thread_id >= MAX_OS_THREADS || !g_scheduler.thread_task_mu_ready) return false;
    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();

    OsThreadEntry* entry = &g_scheduler.os_threads[thread_id];
    if (!entry.active) return false;
    if (!scheduler_os_thread_generation_matches(entry, generation)) return false;

    if (entry.state == OS_THREAD_DONE || entry.state == OS_THREAD_CANCELLED) return false;
    if (entry.cancel_requested) {
        usz waiter_fiber_id = entry.waiter_fiber_id;
        entry.cancel_requested = false;
        entry.state = OS_THREAD_CANCELLED;
        entry.waiter_fiber_id = NO_FIBER;
        if (entry.completion == null) {
            entry.completion = scheduler_make_os_thread_cancel_completion();
        }
        scheduler_signal_thread_task_waiters_locked();
        scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id);
        return false;
    }

    entry.state = OS_THREAD_RUNNING;
    return true;
}

fn void scheduler_complete_os_thread(usz thread_id, uint generation, OffloadCompletion* completion) {
    if (thread_id >= MAX_OS_THREADS || !g_scheduler.thread_task_mu_ready) {
        if (completion != null) scheduler_free_offload_completion(completion);
        return;
    }
    if (completion == null) {
        completion = scheduler_make_os_thread_alloc_failure_completion();
        if (completion == null) return;
    }

    usz waiter_fiber_id = NO_FIBER;
    g_scheduler.thread_task_mu.lock();
    OsThreadEntry* entry = &g_scheduler.os_threads[thread_id];
    if (!entry.active || !scheduler_os_thread_generation_matches(entry, generation)) {
        g_scheduler.thread_task_mu.unlock();
        scheduler_free_offload_completion(completion);
        return;
    }

    if (entry.cancel_requested || entry.state == OS_THREAD_CANCELLED) {
        entry.cancel_requested = false;
        waiter_fiber_id = entry.waiter_fiber_id;
        entry.waiter_fiber_id = NO_FIBER;
        scheduler_free_offload_completion(completion);
        if (entry.completion == null) {
            entry.completion = scheduler_make_os_thread_cancel_completion();
        }
        entry.state = OS_THREAD_CANCELLED;
        g_scheduler.thread_task_mu.unlock();
        scheduler_signal_thread_task_waiters();
        scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id);
        return;
    }

    if (entry.completion != null) scheduler_free_offload_completion(entry.completion);
    entry.completion = completion;
    entry.state = OS_THREAD_DONE;
    waiter_fiber_id = entry.waiter_fiber_id;
    entry.waiter_fiber_id = NO_FIBER;
    g_scheduler.thread_task_mu.unlock();
    scheduler_signal_thread_task_waiters();
    scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id);
}

fn bool scheduler_cancel_os_thread(usz thread_id, bool* out_already_done, uint generation = 0) {
    if (out_already_done != null) *out_already_done = false;
    if (thread_id >= MAX_OS_THREADS || !g_scheduler.thread_task_mu_ready) return false;

    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();
    OsThreadEntry* entry = &g_scheduler.os_threads[thread_id];
    if (!entry.active) return false;
    if (!scheduler_os_thread_generation_matches(entry, generation)) return false;

    if (entry.state == OS_THREAD_DONE || entry.state == OS_THREAD_CANCELLED) {
        if (out_already_done != null) *out_already_done = true;
        return true;
    }

    entry.cancel_requested = true;
    if (entry.state == OS_THREAD_PENDING && !entry.thread_created) {
        usz waiter_fiber_id = entry.waiter_fiber_id;
        entry.cancel_requested = false;
        entry.state = OS_THREAD_CANCELLED;
        entry.waiter_fiber_id = NO_FIBER;
        if (entry.completion == null) {
            entry.completion = scheduler_make_os_thread_cancel_completion();
        }
        scheduler_signal_thread_task_waiters_locked();
        scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id);
    }
    return true;
}
