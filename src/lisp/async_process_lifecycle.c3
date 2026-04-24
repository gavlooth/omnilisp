module lisp;

import std::core::mem;
import main;

struct ProcessWaitOffloadCtx {
    void* uv_process;
    int wait_status;
    int exit_code;
    int term_signal;
}

fn void close_process_handle(ProcessHandle* ph) {
    if (ph == null) return;
    ph.open = false;
    ph.lifecycle_state = PROCESS_HANDLE_STATE_CLOSED;
    if (ph.uv_process == null) return;
    omni_uv_process_close(ph.uv_process);
    ph.uv_process = null;
}

fn void process_handle_store_wait_status(ProcessHandle* ph, int pid, int exit_code, int term_signal) {
    if (ph == null) return;
    ph.wait_pid = pid;
    ph.wait_exit_code = exit_code;
    ph.wait_term_signal = term_signal;
    ph.wait_status_ready = true;
}

fn OffloadCompletion* process_wait_offload_callback(OffloadWork work) {
    ProcessWaitOffloadCtx* wait_ctx = (ProcessWaitOffloadCtx*)work.custom_ctx;

    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) return null;
    *completion = {};

    if (wait_ctx == null || wait_ctx.uv_process == null) {
        scheduler_set_offload_error(completion, "process-wait: invalid or closed process handle");
        return completion;
    }

    wait_ctx.wait_status = omni_uv_process_wait(
        wait_ctx.uv_process,
        &wait_ctx.exit_code,
        &wait_ctx.term_signal
    );
    if (wait_ctx.wait_status < 0) {
        scheduler_set_offload_error(completion, "process-wait: wait failed");
        return completion;
    }

    completion.kind = OFFLOAD_RES_NIL;
    return completion;
}
