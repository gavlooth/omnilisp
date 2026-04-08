module lisp;
import main;
import std::core::mem;

fn Value* scheduler_execute_custom_offload_job(OffloadCustomFn custom_fn, void* custom_ctx, Interp* interp) {
    scheduler_init();

    OffloadWork work = {};
    work.custom_fn = custom_fn;
    work.custom_ctx = custom_ctx;
    work.task_id = NO_TASK;
    return scheduler_execute_offload_work(&work, interp);
}

fn Value* scheduler_begin_custom_offload_task_job(
    OffloadCustomFn custom_fn,
    void* custom_ctx,
    SchedulerTaskHandle* handle,
    Interp* interp
) {
    scheduler_init();

    if (custom_fn == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: invalid custom callback");
    }
    if (handle == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: missing task handle");
    }

    usz task_id = scheduler_alloc_thread_task();
    if (task_id == NO_TASK) {
        return scheduler_raise(interp, "scheduler/offload-task-table-full", "offload: task table full");
    }

    uint generation = 0;
    if (!scheduler_get_thread_task_generation(task_id, &generation) || generation == 0) {
        scheduler_drop_thread_task(task_id);
        return scheduler_raise(interp, "scheduler/offload-task-table-full", "offload: task table full");
    }

    OffloadWork work = {};
    work.custom_fn = custom_fn;
    work.custom_ctx = custom_ctx;
    work.task_id = task_id;
    if (!scheduler_enqueue_offload(work)) {
        scheduler_drop_thread_task(task_id, generation);
        return scheduler_raise(interp, "scheduler/offload-admission-full", "offload: admission cap reached");
    }

    handle.task_id = task_id;
    handle.generation = generation;
    handle.owner_interp = interp;
    return null;
}

fn Value* scheduler_begin_custom_offload_task_jobs(
    OffloadCustomFn custom_fn,
    void** custom_ctxs,
    usz count,
    SchedulerTaskHandle* handles,
    Interp* interp
) {
    scheduler_init();

    if (custom_fn == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: invalid custom callback");
    }
    if (count > 0 && handles == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: missing task handle");
    }
    if (count > 0 && custom_ctxs == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: missing custom context array");
    }

    for (usz i = 0; i < count; i++) {
        handles[i] = {};
    }

    for (usz i = 0; i < count; i++) {
        Value* err = scheduler_begin_custom_offload_task_job(
            custom_fn,
            custom_ctxs != null ? custom_ctxs[i] : null,
            &handles[i],
            interp
        );
        if (err != null) {
            for (usz j = 0; j < i; j++) {
                if (handles[j].task_id != NO_TASK) {
                    scheduler_drop_thread_task(handles[j].task_id, handles[j].generation);
                    handles[j] = {};
                }
            }
            return err;
        }
    }
    return null;
}

fn Value* scheduler_execute_custom_offload_task_jobs(
    OffloadCustomFn custom_fn,
    void** custom_ctxs,
    usz count,
    Interp* interp
) {
    scheduler_init();

    if (custom_fn == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: invalid custom callback");
    }
    if (count > 0 && custom_ctxs == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: missing custom context array");
    }
    if (count == 0) {
        return make_void(interp);
    }

    SchedulerTaskHandle[] handles = mem::new_array(SchedulerTaskHandle, count);
    if (handles.ptr == null) {
        return scheduler_raise(interp, "scheduler/offload-task-table-full", "offload: task table full");
    }
    defer mem::free(handles.ptr);

    Value* begin_err = scheduler_begin_custom_offload_task_jobs(
        custom_fn,
        custom_ctxs,
        count,
        &handles[0],
        interp
    );
    if (begin_err != null) return begin_err;

    for (usz i = 0; i < count; i++) {
        Value* join_v = scheduler_task_join_impl(
            handles[i].task_id,
            handles[i].generation,
            -1,
            &handles[i],
            interp
        );
        if (join_v != null && join_v.tag == ERROR) {
            for (usz j = i + 1; j < count; j++) {
                if (scheduler_thread_task_exists(handles[j].task_id, handles[j].generation)) {
                    scheduler_drop_thread_task(handles[j].task_id, handles[j].generation);
                }
            }
            return join_v;
        }
    }
    return make_void(interp);
}

fn Value* scheduler_execute_custom_offload_task_job(
    OffloadCustomFn custom_fn,
    void* custom_ctx,
    usz task_id,
    Interp* interp
) {
    scheduler_init();

    OffloadWork work = {};
    work.custom_fn = custom_fn;
    work.custom_ctx = custom_ctx;
    work.task_id = task_id;
    return scheduler_execute_offload_work(&work, interp);
}
