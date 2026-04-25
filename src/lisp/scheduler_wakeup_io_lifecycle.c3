module lisp;

import std::core::mem;
import std::io;
import std::thread;
import main;

fn void scheduler_free_handle_cb(void* handle) {
    if (handle != null) mem::free(handle);
}

fn void scheduler_close_pending_io_for_all_fibers() {
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        scheduler_close_pending_tcp_read(i);
        scheduler_close_pending_offload(i);
        scheduler_close_pending_sleep(i);
        scheduler_close_pending_dns(i);
        scheduler_close_pending_tcp_connect(i);
        scheduler_close_pending_tcp_accept(i);
    }
}

fn void scheduler_restore_blocked_fiber_ready(usz fiber_id) {
    if (fiber_id >= g_scheduler.fiber_count) return;
    FiberEntry* f = &g_scheduler.fibers[fiber_id];
    if (f.state == FIBER_BLOCKED) f.state = FIBER_READY;
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

    scheduler_restore_blocked_fiber_ready(fiber_id);
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
    scheduler_restore_blocked_fiber_ready(fiber_id);
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
    scheduler_restore_blocked_fiber_ready(fiber_id);
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
        scheduler_restore_blocked_fiber_ready(fiber_id);
        return;
    }

    PendingDnsCallbackCtx* ctx = scheduler_take_dns_callback_ctx(dns.req_handle);
    if (dns.addrinfo_result != null) {
        omni_uv_freeaddrinfo(dns.addrinfo_result);
    }
    if (dns.req_handle != null) {
        omni_uv_getaddrinfo_req_free(dns.req_handle);
    }
    if (dns.host_copy != null) {
        mem::free(dns.host_copy);
    }
    scheduler_free_dns_callback_ctx(ctx);
    scheduler_restore_blocked_fiber_ready(fiber_id);
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
        scheduler_restore_blocked_fiber_ready(fiber_id);
        return;
    }

    PendingConnectCallbackCtx* ctx = scheduler_take_connect_callback_ctx(pending.req_handle);
    if (pending.req_handle != null) {
        omni_uv_tcp_connect_req_free(pending.req_handle);
    }
    if (pending.tcp_handle != null) {
        omni_uv_tcp_close_and_free(pending.tcp_handle);
    }
    if (ctx != null && ctx.tcp_handle != null && ctx.tcp_handle != pending.tcp_handle) {
        omni_uv_tcp_close_and_free(ctx.tcp_handle);
    }
    scheduler_free_connect_callback_ctx(ctx);
    scheduler_restore_blocked_fiber_ready(fiber_id);
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
    scheduler_restore_blocked_fiber_ready(fiber_id);
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
