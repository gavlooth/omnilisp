module lisp;

const int SCHEDULER_JOIN_WAIT_REGISTERED = 0;
const int SCHEDULER_JOIN_WAIT_READY = 1;
const int SCHEDULER_JOIN_WAIT_TIMEOUT = 2;
const int SCHEDULER_JOIN_WAIT_FAILED = -3;
const int SCHEDULER_JOIN_WAIT_INVALID = -1;
const int SCHEDULER_JOIN_WAIT_CONFLICT = -2;
const int SCHEDULER_CLOCK_MONOTONIC = 1;

fn uint scheduler_next_task_generation_locked() @inline {
    g_scheduler.task_generation_seed++;
    if (g_scheduler.task_generation_seed == 0) g_scheduler.task_generation_seed = 1;
    return g_scheduler.task_generation_seed;
}

fn bool scheduler_task_generation_matches(TaskEntry* task, uint generation) @inline {
    if (generation == 0) return true;
    return task.generation == generation;
}

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
        g_scheduler.thread_tasks[i].generation = scheduler_next_task_generation_locked();
        g_scheduler.thread_tasks[i].request_handle = null;
        g_scheduler.thread_tasks[i].waiter_fiber_id = NO_FIBER;
        g_scheduler.thread_tasks[i].waiter_wake_epoch = 0;
        return i;
    }
    return NO_TASK;
}

fn bool scheduler_get_thread_task_generation(usz task_id, uint* out_generation) {
    if (out_generation != null) *out_generation = 0;
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) return false;

    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();
    TaskEntry* task = &g_scheduler.thread_tasks[task_id];
    if (!task.active) return false;
    if (out_generation != null) *out_generation = task.generation;
    return true;
}

fn bool scheduler_bind_thread_task_request(usz task_id, void* req_handle, uint* out_generation = null) {
    if (out_generation != null) *out_generation = 0;
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) return false;
    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();

    TaskEntry* task = &g_scheduler.thread_tasks[task_id];
    if (!task.active || task.state != THREAD_TASK_PENDING) return false;
    task.request_handle = req_handle;
    if (out_generation != null) *out_generation = task.generation;
    return true;
}

fn void scheduler_clear_thread_task_request(usz task_id, void* expected_req = null, uint generation = 0) {
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) return;
    g_scheduler.thread_task_mu.lock();
    TaskEntry* task = &g_scheduler.thread_tasks[task_id];
    if (task.active) {
        if (!scheduler_task_generation_matches(task, generation)) {
            g_scheduler.thread_task_mu.unlock();
            return;
        }
        if (expected_req == null || task.request_handle == expected_req) {
            task.request_handle = null;
        }
    }
    g_scheduler.thread_task_mu.unlock();
}

fn OffloadCompletion* scheduler_take_thread_task_completion(usz task_id, uint generation = 0) {
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) return null;
    if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
        (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
    }

    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();
    if (!g_scheduler.thread_tasks[task_id].active) {
        return null;
    }
    TaskEntry* task = &g_scheduler.thread_tasks[task_id];
    if (!scheduler_task_generation_matches(task, generation)) return null;
    if (task.state != THREAD_TASK_DONE && task.state != THREAD_TASK_CANCELLED) {
        return null;
    }

    OffloadCompletion* completion = task.completion;
    usz waiter_fiber_id = task.waiter_fiber_id;
    uint waiter_wake_epoch = task.waiter_wake_epoch;
    task.waiter_fiber_id = NO_FIBER;
    task.waiter_wake_epoch = 0;
    (void)scheduler_signal_task_waiter_fiber(waiter_fiber_id, waiter_wake_epoch, true);
    g_scheduler.thread_tasks[task_id] = {};
    scheduler_signal_thread_task_waiters_locked();
    return completion;
}

fn void scheduler_drop_thread_task(usz task_id, uint generation = 0) {
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) return;
    g_scheduler.thread_task_mu.lock();
    OffloadCompletion* completion = null;
    usz waiter_fiber_id = NO_FIBER;
    uint waiter_wake_epoch = 0;
    if (g_scheduler.thread_tasks[task_id].active) {
        TaskEntry* task = &g_scheduler.thread_tasks[task_id];
        if (scheduler_task_generation_matches(task, generation)) {
            completion = task.completion;
            waiter_fiber_id = task.waiter_fiber_id;
            waiter_wake_epoch = task.waiter_wake_epoch;
            g_scheduler.thread_tasks[task_id] = {};
        }
    }
    g_scheduler.thread_task_mu.unlock();
    if (completion != null) scheduler_free_offload_completion(completion);
    scheduler_signal_thread_task_waiters();
    (void)scheduler_signal_task_waiter_fiber(waiter_fiber_id, waiter_wake_epoch);
}

fn bool scheduler_thread_task_exists(usz task_id, uint generation = 0) {
    if (task_id >= MAX_THREAD_TASKS || !g_scheduler.thread_task_mu_ready) return false;
    g_scheduler.thread_task_mu.lock();
    bool exists = g_scheduler.thread_tasks[task_id].active &&
                  scheduler_task_generation_matches(&g_scheduler.thread_tasks[task_id], generation);
    g_scheduler.thread_task_mu.unlock();
    return exists;
}

fn void scheduler_cancel_all_thread_tasks() {
    if (!g_scheduler.thread_task_mu_ready) return;
    for (usz i = 0; i < MAX_THREAD_TASKS; i++) {
        if (!g_scheduler.thread_tasks[i].active) continue;
        bool already_done = false;
        (void)scheduler_cancel_thread_task(i, &already_done, g_scheduler.thread_tasks[i].generation);
    }
}

fn void scheduler_drain_thread_tasks() {
    if (!g_scheduler.thread_task_mu_ready) return;
    for (usz i = 0; i < MAX_THREAD_TASKS; i++) {
        if (!g_scheduler.thread_tasks[i].active) continue;
        scheduler_drop_thread_task(i, g_scheduler.thread_tasks[i].generation);
    }
}
