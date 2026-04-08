module lisp;

import std::core::mem;

fn void scheduler_task_handle_finalizer(void* handle_ptr) {
    SchedulerTaskHandle* handle = (SchedulerTaskHandle*)handle_ptr;
    if (handle == null) return;
    if (handle.task_id < MAX_THREAD_TASKS && handle.generation != 0) {
        scheduler_drop_thread_task(handle.task_id, handle.generation);
    }
    mem::free(handle);
}

fn Value* scheduler_make_task_handle_value(usz task_id, uint generation, Interp* interp) {
    SchedulerTaskHandle* handle = (SchedulerTaskHandle*)mem::malloc(SchedulerTaskHandle.sizeof);
    if (handle == null) {
        scheduler_drop_thread_task(task_id, generation);
        return scheduler_raise(
            interp,
            "scheduler/task-spawn-out-of-memory",
            "task-spawn: out of memory"
        );
    }
    handle.task_id = task_id;
    handle.generation = generation;
    handle.owner_interp = interp;

    Value* out = make_ffi_handle_ex(
        interp,
        handle,
        "task-handle",
        &scheduler_task_handle_finalizer,
        false,
        false
    );
    if (out == null) {
        scheduler_task_handle_finalizer(handle);
        return scheduler_raise(
            interp,
            "scheduler/task-spawn-out-of-memory",
            "task-spawn: out of memory"
        );
    }
    return out;
}

fn Value* scheduler_parse_task_handle_arg(
    Value* arg,
    char[] expected_code,
    char[] expected_msg,
    char[] invalid_code,
    char[] invalid_msg,
    usz* task_id_out,
    uint* generation_out,
    SchedulerTaskHandle** handle_out,
    Interp* interp
) {
    if (task_id_out != null) *task_id_out = NO_TASK;
    if (generation_out != null) *generation_out = 0;
    if (handle_out != null) *handle_out = null;

    if (arg == null || arg.tag != FFI_HANDLE || arg.ffi_val == null || arg.ffi_val.lib_handle == null) {
        return scheduler_raise(interp, expected_code, expected_msg);
    }
    FfiHandle* box = arg.ffi_val;
    if (!scheduler_str_eq(box.lib_name[:box.name_len], "task-handle")) {
        return scheduler_raise(interp, expected_code, expected_msg);
    }

    SchedulerTaskHandle* handle = (SchedulerTaskHandle*)box.lib_handle;
    if (handle == null || handle.task_id >= MAX_THREAD_TASKS || handle.generation == 0) {
        return scheduler_raise(interp, invalid_code, invalid_msg);
    }
    if (handle.owner_interp != null && handle.owner_interp != interp) {
        return scheduler_raise(interp, invalid_code, invalid_msg);
    }

    if (task_id_out != null) *task_id_out = handle.task_id;
    if (generation_out != null) *generation_out = handle.generation;
    if (handle_out != null) *handle_out = handle;
    return null;
}
