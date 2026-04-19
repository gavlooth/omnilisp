module lisp;

fn Value* scheduler_admit_os_thread_work(
    OffloadWork work,
    SchedulerOsThreadHandle* handle,
    Interp* interp
) {
    if (handle == null) {
        scheduler_release_shared(work.shared);
        return scheduler_raise(interp, "scheduler/thread-spawn-invalid-job", "thread-spawn: missing thread handle");
    }

    usz thread_id = scheduler_alloc_os_thread();
    if (thread_id == NO_OS_THREAD) {
        scheduler_release_shared(work.shared);
        return scheduler_raise(interp, "scheduler/thread-spawn-thread-table-full", "thread-spawn: thread table full");
    }

    uint generation = 0;
    if (!scheduler_get_os_thread_generation(thread_id, &generation) || generation == 0) {
        scheduler_drop_os_thread(thread_id);
        scheduler_release_shared(work.shared);
        return scheduler_raise(
            interp,
            "scheduler/thread-spawn-thread-handle-invalid",
            "thread-spawn: failed to create thread handle"
        );
    }

    work.fiber_id = NO_FIBER;
    work.task_id = NO_TASK;
    if (!scheduler_start_os_thread(thread_id, generation, work)) {
        scheduler_drop_os_thread(thread_id, generation);
        scheduler_release_shared(work.shared);
        return scheduler_raise(
            interp,
            "scheduler/thread-spawn-create-failed",
            "thread-spawn: failed to start OS thread"
        );
    }

    handle.thread_id = thread_id;
    handle.generation = generation;
    handle.owner_interp = interp;
    return null;
}

fn void scheduler_rollback_os_thread_handles(
    SchedulerOsThreadHandle* handles,
    usz count
) {
    for (usz i = 0; i < count; i++) {
        if (handles[i].thread_id != NO_OS_THREAD) {
            scheduler_drop_os_thread(handles[i].thread_id, handles[i].generation);
            handles[i] = {};
        }
    }
}

fn void scheduler_drop_existing_os_thread_handles(
    SchedulerOsThreadHandle* handles,
    usz start_index,
    usz count
) {
    for (usz i = start_index; i < count; i++) {
        if (scheduler_os_thread_exists(handles[i].thread_id, handles[i].generation)) {
            scheduler_drop_os_thread(handles[i].thread_id, handles[i].generation);
        }
    }
}

fn Value* scheduler_join_os_thread_handles(
    SchedulerOsThreadHandle* handles,
    usz count,
    Interp* interp
) {
    for (usz i = 0; i < count; i++) {
        Value* join_v = scheduler_thread_join_impl(
            handles[i].thread_id,
            handles[i].generation,
            -1,
            &handles[i],
            interp
        );
        if (join_v != null && join_v.tag == ERROR) {
            scheduler_drop_existing_os_thread_handles(handles, i + 1, count);
            return join_v;
        }
    }
    return null;
}
