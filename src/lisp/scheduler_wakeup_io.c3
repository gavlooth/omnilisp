module lisp;

import std::core::mem;
import std::atomic::types;
import std::io;
import std::thread;
import main;


fn Value* scheduler_value_from_pending_tcp_read(PendingTcpRead* op, Interp* interp) {
    if (op.write_mode) {
        if (op.error_code != 0) {
            return scheduler_raise(interp, "scheduler/tcp-write-async-write-failed", "tcp-write: async write failed");
        }
        return make_int(interp, op.nwritten);
    }
    if (op.timed_out) {
        return scheduler_raise(interp, "scheduler/tcp-read-timeout", "tcp-read: timeout");
    }
    if (op.error_code != 0) {
        return scheduler_raise(interp, "scheduler/tcp-read-async-read-failed", "tcp-read: async read failed");
    }
    if (op.nread <= 0) {
        return make_string(interp, "");
    }
    return make_string(interp, op.buffer[:(usz)op.nread]);
}

fn Value* scheduler_consume_pending_tcp_read(usz fiber_id, Interp* interp) {
    PendingTcpRead* op = &g_scheduler.pending_reads[fiber_id];
    if (!op.active || !op.completed) {
        return scheduler_raise(
            interp,
            "scheduler/tcp-read-resumed-before-async-read-completed",
            "tcp-read: resumed before async read completed"
        );
    }

    Value* out = scheduler_value_from_pending_tcp_read(op, interp);

    op.consumed = true;
    if (op.buffer != null) mem::free(op.buffer);
    if (op.write_buffer != null) mem::free(op.write_buffer);
    g_scheduler.pending_reads[fiber_id] = {};
    if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
        (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
    }

    return out;
}

fn Value* scheduler_value_from_offload_bytes(OffloadCompletion* completion, Interp* interp) {
    return scheduler_domain_project_shared_to_local_value(
        scheduler_current_domain(),
        &completion.shared,
        interp
    );
}

fn Value* scheduler_value_from_offload_error(OffloadCompletion* completion, Interp* interp) {
    if (completion.error_msg != null && completion.error_len > 0) {
        return scheduler_raise(
            interp,
            "scheduler/offload-worker-error",
            completion.error_msg[:completion.error_len]
        );
    }
    return scheduler_raise(interp, "scheduler/offload-worker-failed", "offload: worker failed");
}

fn Value* scheduler_value_from_offload_completion(OffloadCompletion* completion, Interp* interp) {
    if (completion == null) return scheduler_raise(interp, "scheduler/offload-missing-completion", "offload: missing completion");

    switch (completion.kind) {
        case OFFLOAD_RES_NIL:
            return make_nil(interp);
        case OFFLOAD_RES_INT:
            return scheduler_domain_consume_sendable_int(
                scheduler_current_domain(),
                completion.int_result,
                interp
            );
        case OFFLOAD_RES_DOUBLE:
            return make_double(interp, completion.double_result);
        case OFFLOAD_RES_BYTES:
            return scheduler_value_from_offload_bytes(completion, interp);
        case OFFLOAD_RES_ERROR:
            return scheduler_value_from_offload_error(completion, interp);
        default:
            return scheduler_raise(
                interp,
                "scheduler/offload-invalid-completion-kind",
                "offload: invalid completion kind"
            );
    }
}

fn Value* scheduler_consume_pending_offload(usz fiber_id, Interp* interp) {
    PendingOffload* off = &g_scheduler.pending_offloads[fiber_id];
    if (!off.active || !off.completed) {
        return scheduler_raise(
            interp,
            "scheduler/offload-resumed-before-completion",
            "offload: resumed before completion"
        );
    }

    OffloadCompletion* completion = (OffloadCompletion*)off.completion;
    Value* out = scheduler_value_from_offload_completion(completion, interp);

    if (completion != null) scheduler_free_offload_completion(completion);
    g_scheduler.pending_offloads[fiber_id] = {};
    return out;
}

fn Value* scheduler_consume_pending_sleep(usz fiber_id, Interp* interp) {
    PendingSleep* sleep = &g_scheduler.pending_sleeps[fiber_id];
    if (!sleep.active || !sleep.completed) {
        return scheduler_raise(
            interp,
            "scheduler/async-sleep-resumed-before-completion",
            "async-sleep: resumed before timer completion"
        );
    }

    g_scheduler.pending_sleeps[fiber_id] = {};
    return make_void(interp);
}
