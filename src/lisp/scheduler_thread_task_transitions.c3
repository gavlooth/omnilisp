module lisp;

fn OffloadCompletion* scheduler_make_task_cancel_completion() {
    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) return null;
    *completion = {};
    scheduler_set_offload_error(completion, "thread task cancelled");
    return completion;
}

fn OffloadCompletion* scheduler_make_task_alloc_failure_completion() {
    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) return null;
    *completion = {};
    scheduler_set_offload_error(completion, "thread task: worker completion alloc failed");
    return completion;
}

fn void scheduler_prepare_cancel_completion_noalloc(OffloadCompletion* completion, char[] cancel_message) @inline {
    if (completion == null) return;
    if (completion.shared != null) {
        scheduler_release_shared(completion.shared);
        completion.shared = null;
    }
    if (completion.error_msg != null && completion.error_len >= cancel_message.len) {
        for (usz i = 0; i < cancel_message.len; i++) {
            completion.error_msg[i] = cancel_message[i];
        }
        completion.error_len = cancel_message.len;
        completion.kind = OFFLOAD_RES_ERROR;
        return;
    }
    if (completion.error_msg != null) mem::free(completion.error_msg);
    completion.error_msg = null;
    completion.error_len = 0;
    completion.kind = OFFLOAD_RES_ERROR;
    scheduler_set_offload_error(completion, cancel_message);
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
        task.waiter_fiber_id = NO_FIBER;
        task.waiter_wake_epoch = 0;
        if (task.completion == null) {
            task.completion = scheduler_make_task_cancel_completion();
        }
        if (task.completion == null) {
            // Fail closed: never leave CANCELLED entries without completion payload.
            *task = {};
            scheduler_signal_thread_task_waiters_locked();
            (void)scheduler_signal_task_waiter_fiber(waiter_fiber_id, waiter_wake_epoch, true);
            return false;
        }
        task.state = THREAD_TASK_CANCELLED;
        scheduler_signal_thread_task_waiters_locked();
        (void)scheduler_signal_task_waiter_fiber(waiter_fiber_id, waiter_wake_epoch, true);
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
        waiter_fiber_id = task.waiter_fiber_id;
        waiter_wake_epoch = task.waiter_wake_epoch;
        task.waiter_fiber_id = NO_FIBER;
        task.waiter_wake_epoch = 0;
        if (task.completion == null) {
            if (completion != null) {
                scheduler_prepare_cancel_completion_noalloc(completion, "thread task cancelled");
                task.completion = completion;
                completion = null;
            } else {
                task.completion = scheduler_make_task_cancel_completion();
            }
        } else {
            scheduler_free_offload_completion(completion);
            completion = null;
        }
        if (task.completion == null) {
            // Fail closed on OOM instead of publishing CANCELLED without completion.
            *task = {};
            g_scheduler.thread_task_mu.unlock();
            scheduler_signal_thread_task_waiters();
            (void)scheduler_signal_task_waiter_fiber(waiter_fiber_id, waiter_wake_epoch);
            return;
        }
        task.state = THREAD_TASK_CANCELLED;
        g_scheduler.thread_task_mu.unlock();
        scheduler_signal_thread_task_waiters();
        (void)scheduler_signal_task_waiter_fiber(waiter_fiber_id, waiter_wake_epoch);
        return;
    }

    if (task.completion != null) scheduler_free_offload_completion(task.completion);
    task.completion = completion;
    task.state = THREAD_TASK_DONE;
    task.request_handle = null;
    waiter_fiber_id = task.waiter_fiber_id;
    waiter_wake_epoch = task.waiter_wake_epoch;
    task.waiter_fiber_id = NO_FIBER;
    task.waiter_wake_epoch = 0;
    g_scheduler.thread_task_mu.unlock();
    scheduler_signal_thread_task_waiters();
    (void)scheduler_signal_task_waiter_fiber(waiter_fiber_id, waiter_wake_epoch);
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
        task.waiter_fiber_id = NO_FIBER;
        task.waiter_wake_epoch = 0;
        if (task.completion == null) {
            task.completion = scheduler_make_task_cancel_completion();
        }
        if (task.completion == null) {
            // Fail closed: clear entry instead of entering CANCELLED with null completion.
            *task = {};
            scheduler_signal_thread_task_waiters_locked();
            (void)scheduler_signal_task_waiter_fiber(waiter_fiber_id, waiter_wake_epoch, true);
            return true;
        }
        task.state = THREAD_TASK_CANCELLED;
        scheduler_signal_thread_task_waiters_locked();
        (void)scheduler_signal_task_waiter_fiber(waiter_fiber_id, waiter_wake_epoch, true);
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
        uint waiter_wake_epoch = entry.waiter_wake_epoch;
        entry.cancel_requested = false;
        entry.waiter_fiber_id = NO_FIBER;
        entry.waiter_wake_epoch = 0;
        if (entry.completion == null) {
            entry.completion = scheduler_make_os_thread_cancel_completion();
        }
        if (entry.completion == null) {
            *entry = {};
            scheduler_signal_thread_task_waiters_locked();
            (void)scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id, waiter_wake_epoch, true);
            return false;
        }
        entry.state = OS_THREAD_CANCELLED;
        scheduler_signal_thread_task_waiters_locked();
        (void)scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id, waiter_wake_epoch, true);
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
    uint waiter_wake_epoch = 0;
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
        waiter_wake_epoch = entry.waiter_wake_epoch;
        entry.waiter_fiber_id = NO_FIBER;
        entry.waiter_wake_epoch = 0;
        if (entry.completion == null) {
            if (completion != null) {
                scheduler_prepare_cancel_completion_noalloc(completion, "thread cancelled");
                entry.completion = completion;
                completion = null;
            } else {
                entry.completion = scheduler_make_os_thread_cancel_completion();
            }
        } else {
            scheduler_free_offload_completion(completion);
            completion = null;
        }
        if (entry.completion == null) {
            *entry = {};
            g_scheduler.thread_task_mu.unlock();
            scheduler_signal_thread_task_waiters();
            (void)scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id, waiter_wake_epoch);
            return;
        }
        entry.state = OS_THREAD_CANCELLED;
        g_scheduler.thread_task_mu.unlock();
        scheduler_signal_thread_task_waiters();
        (void)scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id, waiter_wake_epoch);
        return;
    }

    if (entry.completion != null) scheduler_free_offload_completion(entry.completion);
    entry.completion = completion;
    entry.state = OS_THREAD_DONE;
    waiter_fiber_id = entry.waiter_fiber_id;
    waiter_wake_epoch = entry.waiter_wake_epoch;
    entry.waiter_fiber_id = NO_FIBER;
    entry.waiter_wake_epoch = 0;
    g_scheduler.thread_task_mu.unlock();
    scheduler_signal_thread_task_waiters();
    (void)scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id, waiter_wake_epoch);
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
        uint waiter_wake_epoch = entry.waiter_wake_epoch;
        entry.cancel_requested = false;
        entry.waiter_fiber_id = NO_FIBER;
        entry.waiter_wake_epoch = 0;
        if (entry.completion == null) {
            entry.completion = scheduler_make_os_thread_cancel_completion();
        }
        if (entry.completion == null) {
            *entry = {};
            scheduler_signal_thread_task_waiters_locked();
            (void)scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id, waiter_wake_epoch, true);
            return true;
        }
        entry.state = OS_THREAD_CANCELLED;
        scheduler_signal_thread_task_waiters_locked();
        (void)scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id, waiter_wake_epoch, true);
    }
    return true;
}
