module lisp;

import std::core::mem;
import std::atomic::types;
import std::io;
import std::thread;
import main;

fn isz scheduler_find_pending_by_handle(void* handle) {
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        PendingTcpRead* op = &g_scheduler.pending_reads[i];
        if (!op.active) continue;
        if (op.poll_handle == handle || op.timer_handle == handle) return (isz)i;
    }
    return -1;
}

fn isz scheduler_find_pending_sleep_by_handle(void* handle) {
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        PendingSleep* sleep = &g_scheduler.pending_sleeps[i];
        if (!sleep.active) continue;
        if (sleep.timer_handle == handle) return (isz)i;
    }
    return -1;
}

fn isz scheduler_find_pending_dns_by_req(void* req_handle) {
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        PendingDnsResolve* dns = &g_scheduler.pending_dns[i];
        if (dns.req_handle == req_handle) return (isz)i;
    }
    return -1;
}

fn isz scheduler_find_pending_tcp_connect_by_req(void* req_handle) {
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        PendingTcpConnect* pending = &g_scheduler.pending_connects[i];
        if (pending.req_handle == req_handle) return (isz)i;
    }
    return -1;
}

fn isz scheduler_find_pending_tcp_accept_by_server(void* server_handle) {
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        PendingTcpAccept* pending = &g_scheduler.pending_accepts[i];
        if (pending.server_handle == server_handle) return (isz)i;
    }
    return -1;
}

fn void scheduler_free_handle_cb(void* handle) {
    if (handle != null) mem::free(handle);
}

fn void scheduler_close_pending_tcp_read(usz fiber_id) {
    if (fiber_id >= g_scheduler.fiber_count) return;

    PendingTcpRead* op = &g_scheduler.pending_reads[fiber_id];
    if (!op.active) return;

    if (op.poll_handle != null) {
        (void)uv_poll_stop(op.poll_handle);
        uv_close(op.poll_handle, &scheduler_free_handle_cb);
        op.poll_handle = null;
    }
    if (op.timer_handle != null) {
        (void)uv_timer_stop(op.timer_handle);
        uv_close(op.timer_handle, &scheduler_free_handle_cb);
        op.timer_handle = null;
    }

    if (op.buffer != null) {
        mem::free(op.buffer);
        op.buffer = null;
    }
    if (op.write_buffer != null) {
        mem::free(op.write_buffer);
        op.write_buffer = null;
    }

    g_scheduler.pending_reads[fiber_id] = {};
}

fn void scheduler_close_pending_offload(usz fiber_id) {
    if (fiber_id >= g_scheduler.fiber_count) return;
    PendingOffload* off = &g_scheduler.pending_offloads[fiber_id];
    if (!off.active) return;

    if (off.completion != null) {
        scheduler_free_offload_completion(off.completion);
        off.completion = null;
    }
    g_scheduler.pending_offloads[fiber_id] = {};
}

fn void scheduler_close_pending_sleep(usz fiber_id) {
    if (fiber_id >= g_scheduler.fiber_count) return;
    PendingSleep* sleep = &g_scheduler.pending_sleeps[fiber_id];
    if (!sleep.active) return;

    if (sleep.timer_handle != null) {
        (void)uv_timer_stop(sleep.timer_handle);
        uv_close(sleep.timer_handle, &scheduler_free_handle_cb);
        sleep.timer_handle = null;
    }
    g_scheduler.pending_sleeps[fiber_id] = {};
}

fn void scheduler_close_pending_dns(usz fiber_id) {
    if (fiber_id >= g_scheduler.fiber_count) return;
    PendingDnsResolve* dns = &g_scheduler.pending_dns[fiber_id];
    if (!dns.active && dns.req_handle == null) return;

    bool req_completed = dns.completed;

    if (!req_completed && dns.req_handle != null) {
        // Let libuv callback own final req/result cleanup when cancellation races completion.
        (void)omni_uv_cancel_req(dns.req_handle);
        dns.active = false;
        dns.completed = false;
        dns.status = 0;
        dns.addrinfo_result = null;
        dns.host_copy = null;
        return;
    }

    if (dns.addrinfo_result != null) {
        omni_uv_freeaddrinfo(dns.addrinfo_result);
    }
    if (dns.req_handle != null) {
        omni_uv_getaddrinfo_req_free(dns.req_handle);
    }
    if (dns.host_copy != null) {
        mem::free(dns.host_copy);
    }
    g_scheduler.pending_dns[fiber_id] = {};
}

fn void scheduler_close_pending_tcp_connect(usz fiber_id) {
    if (fiber_id >= g_scheduler.fiber_count) return;
    PendingTcpConnect* pending = &g_scheduler.pending_connects[fiber_id];
    if (!pending.active && pending.req_handle == null && pending.tcp_handle == null) return;

    bool req_completed = pending.completed;
    if (!req_completed && pending.req_handle != null) {
        // Let libuv callback own final request/handle cleanup when cancellation
        // races completion.
        (void)omni_uv_cancel_req(pending.req_handle);
        pending.active = false;
        pending.completed = false;
        pending.status = 0;
        return;
    }

    if (pending.req_handle != null) {
        omni_uv_tcp_connect_req_free(pending.req_handle);
    }
    if (pending.tcp_handle != null) {
        omni_uv_tcp_close_and_free(pending.tcp_handle);
    }
    g_scheduler.pending_connects[fiber_id] = {};
}

fn void scheduler_close_pending_tcp_accept(usz fiber_id) {
    if (fiber_id >= g_scheduler.fiber_count) return;
    PendingTcpAccept* pending = &g_scheduler.pending_accepts[fiber_id];
    if (!pending.active &&
        pending.server_handle == null &&
        pending.client_handle == null &&
        !pending.has_accepted_fd) return;

    if (pending.server_handle != null && pending.borrowed_server_handle) {
        (void)omni_uv_pipe_listener_set_noop(pending.server_handle);
    }
    if (pending.server_handle != null && !pending.borrowed_server_handle) {
        omni_uv_tcp_close_and_free(pending.server_handle);
    }
    if (pending.client_handle != null) {
        omni_uv_tcp_close_and_free(pending.client_handle);
    }
    if (pending.has_accepted_fd && pending.accepted_fd >= 0) {
        c_close_fd(pending.accepted_fd);
    }
    g_scheduler.pending_accepts[fiber_id] = {};
}

fn void scheduler_uv_close_cb(void* handle) {
    isz idx = scheduler_find_pending_by_handle(handle);
    if (idx < 0) {
        if (handle != null) mem::free(handle);
        return;
    }

    PendingTcpRead* op = &g_scheduler.pending_reads[(usz)idx];
    if (handle != null) mem::free(handle);
    if (op.poll_handle == handle) op.poll_handle = null;
    if (op.timer_handle == handle) op.timer_handle = null;
}

fn void scheduler_complete_pending_tcp_read(usz fiber_id) {
    PendingTcpRead* op = &g_scheduler.pending_reads[fiber_id];
    if (!op.active) return;

    if (op.poll_handle != null) {
        (void)uv_poll_stop(op.poll_handle);
        uv_close(op.poll_handle, &scheduler_uv_close_cb);
    }
    if (op.timer_handle != null) {
        (void)uv_timer_stop(op.timer_handle);
        uv_close(op.timer_handle, &scheduler_uv_close_cb);
    }

    FiberEntry* f = &g_scheduler.fibers[fiber_id];
    if (f.state == FIBER_BLOCKED) f.state = FIBER_READY;
}

fn void scheduler_complete_pending_offload(usz fiber_id) {
    if (fiber_id >= g_scheduler.fiber_count) return;
    PendingOffload* off = &g_scheduler.pending_offloads[fiber_id];
    if (!off.active) return;

    FiberEntry* f = &g_scheduler.fibers[fiber_id];
    if (f.state == FIBER_BLOCKED) f.state = FIBER_READY;
}

fn void scheduler_complete_pending_sleep(usz fiber_id) {
    if (fiber_id >= g_scheduler.fiber_count) return;
    PendingSleep* sleep = &g_scheduler.pending_sleeps[fiber_id];
    if (!sleep.active) return;

    if (sleep.timer_handle != null) {
        (void)uv_timer_stop(sleep.timer_handle);
        uv_close(sleep.timer_handle, &scheduler_free_handle_cb);
        sleep.timer_handle = null;
    }

    FiberEntry* f = &g_scheduler.fibers[fiber_id];
    if (f.state == FIBER_BLOCKED) f.state = FIBER_READY;
}

fn void scheduler_complete_pending_dns(usz fiber_id) {
    if (fiber_id >= g_scheduler.fiber_count) return;
    PendingDnsResolve* dns = &g_scheduler.pending_dns[fiber_id];
    if (!dns.active) return;

    FiberEntry* f = &g_scheduler.fibers[fiber_id];
    if (f.state == FIBER_BLOCKED) f.state = FIBER_READY;
}

fn void scheduler_complete_pending_tcp_connect(usz fiber_id) {
    if (fiber_id >= g_scheduler.fiber_count) return;
    PendingTcpConnect* pending = &g_scheduler.pending_connects[fiber_id];
    if (!pending.active) return;

    FiberEntry* f = &g_scheduler.fibers[fiber_id];
    if (f.state == FIBER_BLOCKED) f.state = FIBER_READY;
}

fn void scheduler_complete_pending_tcp_accept(usz fiber_id) {
    if (fiber_id >= g_scheduler.fiber_count) return;
    PendingTcpAccept* pending = &g_scheduler.pending_accepts[fiber_id];
    if (!pending.active) return;

    FiberEntry* f = &g_scheduler.fibers[fiber_id];
    if (f.state == FIBER_BLOCKED) f.state = FIBER_READY;
}


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

    Value* out;
    switch (completion.kind) {
        case OFFLOAD_RES_NIL:
            out = make_nil(interp);
        case OFFLOAD_RES_INT:
            out = scheduler_domain_consume_sendable_int(
                scheduler_current_domain(),
                completion.int_result,
                interp
            );
        case OFFLOAD_RES_BYTES:
            out = scheduler_value_from_offload_bytes(completion, interp);
        case OFFLOAD_RES_ERROR:
            out = scheduler_value_from_offload_error(completion, interp);
    }
    return out;
}

fn Value* scheduler_consume_pending_offload(usz fiber_id, Interp* interp) {
    PendingOffload* off = &g_scheduler.pending_offloads[fiber_id];
    if (!off.active || !off.completed || off.completion == null) {
        return scheduler_raise(
            interp,
            "scheduler/offload-resumed-before-completion",
            "offload: resumed before completion"
        );
    }

    OffloadCompletion* completion = (OffloadCompletion*)off.completion;
    Value* out = scheduler_value_from_offload_completion(completion, interp);

    scheduler_free_offload_completion(off.completion);
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
