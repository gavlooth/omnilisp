module lisp;
import main;
import std::core::mem;

fn Value* scheduler_begin_offload_work_job(
    OffloadWork work,
    SchedulerTaskHandle* handle,
    Interp* interp
) {
    scheduler_init();

    if (handle == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: missing task handle");
    }
    return scheduler_admit_offload_task_work(
        &work,
        true,
        false,
        false,
        "scheduler/offload-task-table-full",
        "offload: task table full",
        handle,
        interp
    );
}

fn Value* scheduler_begin_offload_work_jobs(
    OffloadWork[] works,
    SchedulerTaskHandle* handles,
    Interp* interp
) {
    scheduler_init();

    if (works.len > 0 && handles == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: missing task handle");
    }

    for (usz i = 0; i < works.len; i++) {
        handles[i] = {};
    }

    for (usz i = 0; i < works.len; i++) {
        Value* err = scheduler_begin_offload_work_job(works[i], &handles[i], interp);
        if (err != null) {
            scheduler_rollback_offload_task_handles(handles, i);
            return err;
        }
    }
    return null;
}

fn Value* scheduler_execute_offload_work_jobs(
    OffloadWork[] works,
    Interp* interp
) {
    scheduler_init();

    if (works.len == 0) {
        return make_void(interp);
    }

    SchedulerTaskHandle[] handles = mem::new_array(SchedulerTaskHandle, works.len);
    if (handles.ptr == null) {
        return scheduler_raise(interp, "scheduler/offload-task-table-full", "offload: task table full");
    }
    defer mem::free(handles.ptr);

    Value* begin_err = scheduler_begin_offload_work_jobs(works, &handles[0], interp);
    if (begin_err != null) return begin_err;

    Value* join_err = scheduler_join_offload_task_handles(&handles[0], works.len, null, interp);
    if (join_err != null) return join_err;

    return make_void(interp);
}
