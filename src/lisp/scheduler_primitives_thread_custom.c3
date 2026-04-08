module lisp;

import std::core::mem;

fn Value* scheduler_begin_custom_os_thread_job(
    OffloadCustomFn custom_fn,
    void* custom_ctx,
    SchedulerOsThreadHandle* handle,
    Interp* interp
) {
    scheduler_init();

    if (custom_fn == null) {
        return scheduler_raise(interp, "scheduler/thread-spawn-invalid-job", "thread-spawn: invalid custom callback");
    }
    if (handle == null) {
        return scheduler_raise(interp, "scheduler/thread-spawn-invalid-job", "thread-spawn: missing thread handle");
    }

    OffloadWork work = {};
    work.custom_fn = custom_fn;
    work.custom_ctx = custom_ctx;
    return scheduler_admit_os_thread_work(work, handle, interp);
}

fn Value* scheduler_begin_custom_os_thread_jobs(
    OffloadCustomFn custom_fn,
    void** custom_ctxs,
    usz count,
    SchedulerOsThreadHandle* handles,
    Interp* interp
) {
    scheduler_init();

    if (custom_fn == null) {
        return scheduler_raise(interp, "scheduler/thread-spawn-invalid-job", "thread-spawn: invalid custom callback");
    }
    if (count > 0 && handles == null) {
        return scheduler_raise(interp, "scheduler/thread-spawn-invalid-job", "thread-spawn: missing thread handle");
    }
    if (count > 0 && custom_ctxs == null) {
        return scheduler_raise(interp, "scheduler/thread-spawn-invalid-job", "thread-spawn: missing custom context array");
    }

    for (usz i = 0; i < count; i++) {
        handles[i] = {};
    }

    for (usz i = 0; i < count; i++) {
        Value* err = scheduler_begin_custom_os_thread_job(
            custom_fn,
            custom_ctxs != null ? custom_ctxs[i] : null,
            &handles[i],
            interp
        );
        if (err != null) {
            scheduler_rollback_os_thread_handles(handles, i);
            return err;
        }
    }
    return null;
}

fn Value* scheduler_execute_custom_os_thread_jobs(
    OffloadCustomFn custom_fn,
    void** custom_ctxs,
    usz count,
    Interp* interp
) {
    scheduler_init();

    if (custom_fn == null) {
        return scheduler_raise(interp, "scheduler/thread-spawn-invalid-job", "thread-spawn: invalid custom callback");
    }
    if (count > 0 && custom_ctxs == null) {
        return scheduler_raise(interp, "scheduler/thread-spawn-invalid-job", "thread-spawn: missing custom context array");
    }
    if (count == 0) {
        return make_void(interp);
    }

    SchedulerOsThreadHandle[] handles = mem::new_array(SchedulerOsThreadHandle, count);
    if (handles.ptr == null) {
        return scheduler_raise(interp, "scheduler/thread-spawn-out-of-memory", "thread-spawn: out of memory");
    }
    defer mem::free(handles.ptr);

    Value* begin_err = scheduler_begin_custom_os_thread_jobs(
        custom_fn,
        custom_ctxs,
        count,
        &handles[0],
        interp
    );
    if (begin_err != null) return begin_err;

    Value* join_err = scheduler_join_os_thread_handles(&handles[0], count, interp);
    if (join_err != null) return join_err;
    return make_void(interp);
}
