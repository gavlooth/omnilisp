module lisp;

fn OffloadCompletion* scheduler_make_os_thread_cancel_completion() {
    return scheduler_make_transition_error_completion("thread cancelled");
}

fn OffloadCompletion* scheduler_make_os_thread_alloc_failure_completion() {
    return scheduler_make_transition_error_completion("thread: worker completion alloc failed");
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
        scheduler_transition_take_waiter(
            &waiter_fiber_id,
            &waiter_wake_epoch,
            &entry.waiter_fiber_id,
            &entry.waiter_wake_epoch
        );
        if (entry.completion == null) {
            entry.completion = scheduler_make_os_thread_cancel_completion();
        }
        if (entry.completion == null) {
            *entry = {};
            scheduler_publish_os_thread_cancel_locked(waiter_fiber_id, waiter_wake_epoch);
            return false;
        }
        entry.state = OS_THREAD_CANCELLED;
        scheduler_publish_os_thread_cancel_locked(waiter_fiber_id, waiter_wake_epoch);
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
        scheduler_transition_take_waiter(
            &waiter_fiber_id,
            &waiter_wake_epoch,
            &entry.waiter_fiber_id,
            &entry.waiter_wake_epoch
        );
        if (!scheduler_transition_materialize_cancel_completion(
            &entry.completion,
            &completion,
            "thread cancelled"
        )) {
            *entry = {};
            g_scheduler.thread_task_mu.unlock();
            scheduler_publish_os_thread_cancel_unlocked(waiter_fiber_id, waiter_wake_epoch);
            return;
        }
        entry.state = OS_THREAD_CANCELLED;
        g_scheduler.thread_task_mu.unlock();
        scheduler_publish_os_thread_cancel_unlocked(waiter_fiber_id, waiter_wake_epoch);
        return;
    }

    scheduler_transition_handoff_completion(
        &entry.completion,
        completion,
        &entry.waiter_fiber_id,
        &entry.waiter_wake_epoch,
        &waiter_fiber_id,
        &waiter_wake_epoch
    );
    entry.state = OS_THREAD_DONE;
    g_scheduler.thread_task_mu.unlock();
    scheduler_publish_os_thread_completion_unlocked(waiter_fiber_id, waiter_wake_epoch);
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
        scheduler_transition_take_waiter(
            &waiter_fiber_id,
            &waiter_wake_epoch,
            &entry.waiter_fiber_id,
            &entry.waiter_wake_epoch
        );
        if (!scheduler_transition_materialize_cancel_completion(
            &entry.completion,
            null,
            "thread cancelled"
        )) {
            *entry = {};
            scheduler_publish_os_thread_cancel_locked(waiter_fiber_id, waiter_wake_epoch);
            return true;
        }
        entry.state = OS_THREAD_CANCELLED;
        scheduler_publish_os_thread_cancel_locked(waiter_fiber_id, waiter_wake_epoch);
    }
    return true;
}
