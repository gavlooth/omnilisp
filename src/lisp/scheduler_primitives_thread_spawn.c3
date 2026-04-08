module lisp;

import std::core::mem;

fn void scheduler_os_thread_handle_finalizer(void* handle_ptr) {
    SchedulerOsThreadHandle* handle = (SchedulerOsThreadHandle*)handle_ptr;
    if (handle == null) return;
    if (handle.thread_id < MAX_OS_THREADS && handle.generation != 0) {
        scheduler_drop_os_thread(handle.thread_id, handle.generation);
    }
    mem::free(handle);
}

fn Value* scheduler_make_thread_handle_value(usz thread_id, uint generation, Interp* interp) {
    SchedulerOsThreadHandle* handle = (SchedulerOsThreadHandle*)mem::malloc(SchedulerOsThreadHandle.sizeof);
    if (handle == null) {
        scheduler_drop_os_thread(thread_id, generation);
        return scheduler_raise(
            interp,
            "scheduler/thread-spawn-out-of-memory",
            "thread-spawn: out of memory"
        );
    }
    handle.thread_id = thread_id;
    handle.generation = generation;
    handle.owner_interp = interp;

    Value* out = make_ffi_handle_ex(
        interp,
        handle,
        "thread-handle",
        &scheduler_os_thread_handle_finalizer,
        false,
        false
    );
    if (out == null) {
        scheduler_os_thread_handle_finalizer(handle);
        return scheduler_raise(
            interp,
            "scheduler/thread-spawn-out-of-memory",
            "thread-spawn: out of memory"
        );
    }
    return out;
}

fn Value* scheduler_parse_thread_handle_arg(
    Value* arg,
    char[] expected_code,
    char[] expected_msg,
    char[] invalid_code,
    char[] invalid_msg,
    usz* thread_id_out,
    uint* generation_out,
    SchedulerOsThreadHandle** handle_out,
    Interp* interp
) {
    if (thread_id_out != null) *thread_id_out = NO_OS_THREAD;
    if (generation_out != null) *generation_out = 0;
    if (handle_out != null) *handle_out = null;

    if (arg == null || arg.tag != FFI_HANDLE || arg.ffi_val == null || arg.ffi_val.lib_handle == null) {
        return scheduler_raise(interp, expected_code, expected_msg);
    }
    FfiHandle* box = arg.ffi_val;
    if (!scheduler_str_eq(box.lib_name[:box.name_len], "thread-handle")) {
        return scheduler_raise(interp, expected_code, expected_msg);
    }

    SchedulerOsThreadHandle* handle = (SchedulerOsThreadHandle*)box.lib_handle;
    if (handle == null || handle.thread_id >= MAX_OS_THREADS || handle.generation == 0) {
        return scheduler_raise(interp, invalid_code, invalid_msg);
    }
    if (handle.owner_interp != null && handle.owner_interp != interp) {
        return scheduler_raise(interp, invalid_code, invalid_msg);
    }

    if (thread_id_out != null) *thread_id_out = handle.thread_id;
    if (generation_out != null) *generation_out = handle.generation;
    if (handle_out != null) *handle_out = handle;
    return null;
}

fn Value* scheduler_prepare_os_thread_spawn(OffloadWork* work, usz* thread_id_out, uint* generation_out, Interp* interp) {
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
    if (!scheduler_start_os_thread(thread_id, generation, *work)) {
        scheduler_drop_os_thread(thread_id, generation);
        scheduler_release_shared(work.shared);
        return scheduler_raise(
            interp,
            "scheduler/thread-spawn-create-failed",
            "thread-spawn: failed to start OS thread"
        );
    }

    *thread_id_out = thread_id;
    *generation_out = generation;
    return null;
}

fn Value* scheduler_begin_os_thread_work_job(
    OffloadWork work,
    SchedulerOsThreadHandle* handle,
    Interp* interp
) {
    scheduler_init();

    return scheduler_admit_os_thread_work(work, handle, interp);
}

fn Value* scheduler_begin_os_thread_work_jobs(
    OffloadWork[] works,
    SchedulerOsThreadHandle* handles,
    Interp* interp
) {
    scheduler_init();

    if (works.len > 0 && handles == null) {
        return scheduler_raise(interp, "scheduler/thread-spawn-invalid-job", "thread-spawn: missing thread handle");
    }

    for (usz i = 0; i < works.len; i++) {
        handles[i] = {};
    }

    for (usz i = 0; i < works.len; i++) {
        Value* err = scheduler_begin_os_thread_work_job(works[i], &handles[i], interp);
        if (err != null) {
            scheduler_rollback_os_thread_handles(handles, i);
            return err;
        }
    }
    return null;
}

fn Value* scheduler_execute_os_thread_work_jobs(
    OffloadWork[] works,
    Interp* interp
) {
    scheduler_init();

    if (works.len == 0) {
        return make_void(interp);
    }

    SchedulerOsThreadHandle[] handles = mem::new_array(SchedulerOsThreadHandle, works.len);
    if (handles.ptr == null) {
        return scheduler_raise(interp, "scheduler/thread-spawn-out-of-memory", "thread-spawn: out of memory");
    }
    defer mem::free(handles.ptr);

    Value* begin_err = scheduler_begin_os_thread_work_jobs(works, &handles[0], interp);
    if (begin_err != null) return begin_err;

    Value* join_err = scheduler_join_os_thread_handles(&handles[0], works.len, interp);
    if (join_err != null) return join_err;
    return make_void(interp);
}
