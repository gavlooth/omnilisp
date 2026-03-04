module lisp;

fn usz scheduler_alloc_thread_task() {
    if (!g_scheduler.thread_task_mu_ready) return NO_TASK;
    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();

    for (usz i = 0; i < MAX_THREAD_TASKS; i++) {
        if (g_scheduler.thread_tasks[i].active) continue;
        g_scheduler.thread_tasks[i] = {};
        g_scheduler.thread_tasks[i].active = true;
        g_scheduler.thread_tasks[i].state = THREAD_TASK_PENDING;
        g_scheduler.thread_tasks[i].cancel_requested = false;
        return i;
    }
    return NO_TASK;
}

fn OffloadCompletion* scheduler_make_task_cancel_completion() {
    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) return null;
    *completion = {};
    scheduler_set_offload_error(completion, "thread task cancelled");
    return completion;
}

fn bool scheduler_try_begin_thread_task(usz task_id) {
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) return false;
    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();

    if (!g_scheduler.thread_tasks[task_id].active) return false;
    ThreadTaskEntry* task = &g_scheduler.thread_tasks[task_id];
    if (task.state == THREAD_TASK_CANCELLED || task.state == THREAD_TASK_DONE) return false;
    if (task.cancel_requested) {
        task.cancel_requested = false;
        task.state = THREAD_TASK_CANCELLED;
        if (task.completion == null) {
            task.completion = scheduler_make_task_cancel_completion();
        }
        return false;
    }

    task.state = THREAD_TASK_RUNNING;
    return true;
}

fn void scheduler_complete_thread_task(usz task_id, OffloadCompletion* completion) {
    if (completion == null) return;
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) {
        scheduler_free_offload_completion(completion);
        return;
    }

    g_scheduler.thread_task_mu.lock();
    if (!g_scheduler.thread_tasks[task_id].active) {
        g_scheduler.thread_task_mu.unlock();
        scheduler_free_offload_completion(completion);
        return;
    }
    ThreadTaskEntry* task = &g_scheduler.thread_tasks[task_id];

    if (task.cancel_requested || task.state == THREAD_TASK_CANCELLED) {
        task.cancel_requested = false;
        scheduler_free_offload_completion(completion);
        if (task.completion == null) {
            task.completion = scheduler_make_task_cancel_completion();
        }
        task.state = THREAD_TASK_CANCELLED;
        g_scheduler.thread_task_mu.unlock();
        return;
    }

    if (task.completion != null) scheduler_free_offload_completion(task.completion);
    task.completion = completion;
    task.state = THREAD_TASK_DONE;
    g_scheduler.thread_task_mu.unlock();
}

fn OffloadCompletion* scheduler_take_thread_task_completion(usz task_id) {
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) return null;

    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();
    if (!g_scheduler.thread_tasks[task_id].active) {
        return null;
    }
    ThreadTaskEntry* task = &g_scheduler.thread_tasks[task_id];
    if (task.state != THREAD_TASK_DONE && task.state != THREAD_TASK_CANCELLED) {
        return null;
    }

    OffloadCompletion* completion = task.completion;
    g_scheduler.thread_tasks[task_id] = {};
    return completion;
}

fn void scheduler_drop_thread_task(usz task_id) {
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) return;
    g_scheduler.thread_task_mu.lock();
    OffloadCompletion* completion = null;
    if (g_scheduler.thread_tasks[task_id].active) {
        completion = g_scheduler.thread_tasks[task_id].completion;
        g_scheduler.thread_tasks[task_id] = {};
    }
    g_scheduler.thread_task_mu.unlock();
    if (completion != null) scheduler_free_offload_completion(completion);
}

fn bool scheduler_thread_task_exists(usz task_id) {
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) return false;
    g_scheduler.thread_task_mu.lock();
    bool exists = g_scheduler.thread_tasks[task_id].active;
    g_scheduler.thread_task_mu.unlock();
    return exists;
}

fn bool scheduler_cancel_thread_task(usz task_id, bool* out_already_done) {
    if (out_already_done != null) *out_already_done = false;
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) return false;

    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();
    ThreadTaskEntry* task = &g_scheduler.thread_tasks[task_id];
    if (!task.active) return false;

    if (task.state == THREAD_TASK_DONE || task.state == THREAD_TASK_CANCELLED) {
        if (out_already_done != null) *out_already_done = true;
        return true;
    }

    if (task.state == THREAD_TASK_PENDING) {
        task.state = THREAD_TASK_CANCELLED;
        task.cancel_requested = false;
        if (task.completion == null) {
            task.completion = scheduler_make_task_cancel_completion();
        }
        return true;
    }

    // Running: cooperative cancel request, completion will convert to cancelled.
    task.cancel_requested = true;
    return true;
}
