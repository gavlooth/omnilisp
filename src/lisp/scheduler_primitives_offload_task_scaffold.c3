module lisp;

fn Value* scheduler_admit_offload_task_work(
    OffloadWork* work,
    bool require_unassigned_task_id,
    bool release_shared_on_error,
    bool normalize_queue_fields,
    char[] generation_error_code,
    char[] generation_error_msg,
    SchedulerTaskHandle* handle,
    Interp* interp
) {
    if (work == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: invalid job");
    }

    usz task_id = work.task_id;
    if (require_unassigned_task_id && task_id != NO_TASK) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: batch work must not carry a task id");
    }

    bool allocated_task_id = false;
    if (task_id == NO_TASK) {
        task_id = scheduler_alloc_thread_task();
        allocated_task_id = true;
    }
    if (task_id == NO_TASK) {
        if (release_shared_on_error) scheduler_release_shared(work.shared);
        scheduler_release_custom_ctx(work);
        return scheduler_raise(interp, "scheduler/offload-task-table-full", "offload: task table full");
    }

    uint generation = 0;
    if (!scheduler_get_thread_task_generation(task_id, &generation) || generation == 0) {
        if (release_shared_on_error) scheduler_release_shared(work.shared);
        scheduler_release_custom_ctx(work);
        if (allocated_task_id) scheduler_drop_thread_task(task_id);
        return scheduler_raise(interp, generation_error_code, generation_error_msg);
    }

    if (normalize_queue_fields) {
        work.fiber_id = NO_FIBER;
        work.wake_epoch = 0;
    }
    work.task_id = task_id;
    if (!scheduler_enqueue_offload(*work)) {
        if (release_shared_on_error) scheduler_release_shared(work.shared);
        scheduler_release_custom_ctx(work);
        scheduler_drop_thread_task(task_id, generation);
        return scheduler_raise(interp, "scheduler/offload-admission-full", "offload: admission cap reached");
    }

    if (handle != null) {
        handle.task_id = task_id;
        handle.generation = generation;
        handle.owner_interp = interp;
    }
    return null;
}

fn void scheduler_rollback_offload_task_handles(SchedulerTaskHandle* handles, usz count) {
    if (handles == null) return;

    for (usz i = 0; i < count; i++) {
        if (handles[i].task_id != NO_TASK) {
            scheduler_drop_thread_task(handles[i].task_id, handles[i].generation);
        }
        handles[i] = {};
    }
}

fn Value* scheduler_join_offload_task_handles(
    SchedulerTaskHandle* handles,
    usz count,
    Value** results,
    Interp* interp
) {
    if (handles == null) return null;

    for (usz i = 0; i < count; i++) {
        Value* join_v = scheduler_task_join_impl(
            handles[i].task_id,
            handles[i].generation,
            -1,
            &handles[i],
            interp
        );
        if (join_v != null && join_v.tag == ERROR) {
            if (i + 1 < count) {
                scheduler_rollback_offload_task_handles(&handles[i + 1], count - (i + 1));
            }
            return join_v;
        }
        if (results != null) {
            if (join_v == null) join_v = make_nil(interp);
            results[i] = join_v;
        }
    }
    return null;
}
