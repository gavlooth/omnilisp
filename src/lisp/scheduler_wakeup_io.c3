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

fn void scheduler_signal_async() @inline {
    if (g_scheduler.async_handle != null) (void)uv_async_send(g_scheduler.async_handle);
}

fn void scheduler_handle_invalid_wakeup(WakeupEvent ev) {
    if (ev.type == WAKEUP_OFFLOAD_READY && ev.payload != null) {
        scheduler_free_offload_completion(ev.payload);
    }
}

fn void scheduler_dispatch_wakeup_event(WakeupEvent ev) {
    usz fid = ev.fiber_id;
    if (fid >= g_scheduler.fiber_count) {
        scheduler_handle_invalid_wakeup(ev);
        return;
    }

    switch (ev.type) {
        case WAKEUP_TIMER_EXPIRED:
            scheduler_handle_wakeup_timer_expired(fid);
        case WAKEUP_POLL_ERROR:
            scheduler_handle_wakeup_poll_error(fid, ev.status);
        case WAKEUP_POLL_READABLE:
            scheduler_handle_wakeup_poll_readable(fid);
        case WAKEUP_OFFLOAD_READY:
            scheduler_handle_wakeup_offload_ready(fid, ev.payload);
        case WAKEUP_SLEEP_DONE:
            scheduler_handle_wakeup_sleep_done(fid);
        case WAKEUP_DNS_DONE:
            scheduler_handle_wakeup_dns_done(fid);
    }
}

fn ReliableWakeupNode* reliable_wakeup_alloc_node(WakeupEvent ev) {
    ReliableWakeupNode* node = (ReliableWakeupNode*)mem::malloc(ReliableWakeupNode.sizeof);
    if (node == null) return null;
    node.event = ev;
    node.next = null;
    return node;
}

fn bool scheduler_publish_reliable_wakeup(WakeupEvent ev) {
    ReliableWakeupQueue* q = &g_scheduler.reliable_wakeups;
    if (!q.initialized) {
        g_scheduler.wakeup_enqueue_failures.add(1);
        return false;
    }

    ReliableWakeupNode* node = reliable_wakeup_alloc_node(ev);
    if (node == null) {
        g_scheduler.wakeup_enqueue_failures.add(1);
        return false;
    }

    q.mu.lock();
    if (q.tail == null) {
        q.head = node;
        q.tail = node;
    } else {
        q.tail.next = node;
        q.tail = node;
    }
    q.count++;
    q.mu.unlock();

    g_scheduler.reliable_wakeup_enqueues.add(1);
    scheduler_signal_async();
    return true;
}

fn ReliableWakeupNode* scheduler_take_reliable_wakeup_batch() {
    ReliableWakeupQueue* q = &g_scheduler.reliable_wakeups;
    if (!q.initialized) return null;

    q.mu.lock();
    ReliableWakeupNode* head = q.head;
    q.head = null;
    q.tail = null;
    q.count = 0;
    q.mu.unlock();
    return head;
}

fn void scheduler_clear_reliable_wakeup_queue() {
    ReliableWakeupNode* node = scheduler_take_reliable_wakeup_batch();
    while (node != null) {
        ReliableWakeupNode* next = node.next;
        scheduler_handle_invalid_wakeup(node.event);
        mem::free(node);
        node = next;
    }
}

fn usz reliable_wakeup_pending_count() {
    ReliableWakeupQueue* q = &g_scheduler.reliable_wakeups;
    if (!q.initialized) return 0;

    q.mu.lock();
    usz count = q.count;
    q.mu.unlock();
    return count;
}

fn bool scheduler_publish_timer_expired(usz fiber_id) @inline {
    WakeupEvent ev = {};
    ev.fiber_id = fiber_id;
    ev.type = WAKEUP_TIMER_EXPIRED;
    return scheduler_publish_reliable_wakeup(ev);
}

fn bool scheduler_publish_poll_error(usz fiber_id, int status) @inline {
    WakeupEvent ev = {};
    ev.fiber_id = fiber_id;
    ev.type = WAKEUP_POLL_ERROR;
    ev.status = status;
    return scheduler_publish_reliable_wakeup(ev);
}

fn bool scheduler_publish_offload_ready(usz fiber_id, OffloadCompletion* completion) @inline {
    WakeupEvent ev = {};
    ev.fiber_id = fiber_id;
    ev.type = WAKEUP_OFFLOAD_READY;
    ev.payload = completion;
    return scheduler_publish_reliable_wakeup(ev);
}

fn bool scheduler_publish_sleep_done(usz fiber_id) @inline {
    WakeupEvent ev = {};
    ev.fiber_id = fiber_id;
    ev.type = WAKEUP_SLEEP_DONE;
    return scheduler_publish_reliable_wakeup(ev);
}

fn bool scheduler_publish_dns_done(usz fiber_id) @inline {
    WakeupEvent ev = {};
    ev.fiber_id = fiber_id;
    ev.type = WAKEUP_DNS_DONE;
    return scheduler_publish_reliable_wakeup(ev);
}

fn void scheduler_signal_poll_readable(usz fiber_id) {
    if (fiber_id >= g_scheduler.fiber_count) return;
    PendingTcpRead* op = &g_scheduler.pending_reads[fiber_id];
    if (!op.active || op.completed) return;
    if (op.write_mode) return;
    if (op.readable_pending) {
        g_scheduler.readable_signal_coalesces.add(1);
        return;
    }

    op.readable_pending = true;
    scheduler_signal_async();
}

fn void scheduler_signal_poll_writable(usz fiber_id) {
    if (fiber_id >= g_scheduler.fiber_count) return;
    PendingTcpRead* op = &g_scheduler.pending_reads[fiber_id];
    if (!op.active || op.completed) return;
    if (!op.write_mode) return;
    if (op.writable_pending) {
        g_scheduler.readable_signal_coalesces.add(1);
        return;
    }

    op.writable_pending = true;
    scheduler_signal_async();
}

fn void scheduler_uv_timer_cb(void* handle) {
    isz idx = scheduler_find_pending_by_handle(handle);
    if (idx < 0) return;

    PendingTcpRead* op = &g_scheduler.pending_reads[(usz)idx];
    if (!op.active || op.completed) return;

    (void)scheduler_publish_timer_expired((usz)idx);
}

fn void scheduler_uv_sleep_timer_cb(void* handle) {
    isz idx = scheduler_find_pending_sleep_by_handle(handle);
    if (idx < 0) return;
    PendingSleep* sleep = &g_scheduler.pending_sleeps[(usz)idx];
    if (!sleep.active || sleep.completed) return;
    (void)scheduler_publish_sleep_done((usz)idx);
}

fn void scheduler_uv_getaddrinfo_cb(void* req, int status, void* res) {
    void* host_copy = omni_uv_req_get_data(req);
    if (host_copy != null) {
        mem::free(host_copy);
        omni_uv_req_set_data(req, null);
    }

    isz idx = scheduler_find_pending_dns_by_req(req);
    if (idx < 0) {
        if (res != null) omni_uv_freeaddrinfo(res);
        omni_uv_getaddrinfo_req_free(req);
        return;
    }

    PendingDnsResolve* dns = &g_scheduler.pending_dns[(usz)idx];
    dns.host_copy = null;

    if (!dns.active || dns.completed) {
        if (res != null) omni_uv_freeaddrinfo(res);
        omni_uv_getaddrinfo_req_free(req);
        dns.req_handle = null;
        dns.addrinfo_result = null;
        dns.completed = false;
        dns.status = status;
        return;
    }

    dns.status = status;
    dns.addrinfo_result = res;
    dns.completed = true;
    if (!scheduler_publish_dns_done((usz)idx)) {
        scheduler_complete_pending_dns((usz)idx);
    }
}

fn void scheduler_uv_poll_cb(void* handle, int status, int events) {
    isz idx = scheduler_find_pending_by_handle(handle);
    if (idx < 0) return;

    PendingTcpRead* op = &g_scheduler.pending_reads[(usz)idx];
    if (!op.active || op.completed) return;

    if (status < 0) {
        (void)scheduler_publish_poll_error((usz)idx, status);
    } else {
        if ((events & UV_READABLE) != 0) {
            scheduler_signal_poll_readable((usz)idx);
        }
        if ((events & UV_WRITABLE) != 0) {
            scheduler_signal_poll_writable((usz)idx);
        }
    }
}

fn void scheduler_handle_wakeup_timer_expired(usz fid) {
    PendingTcpRead* op = &g_scheduler.pending_reads[fid];
    if (!op.active || op.completed) return;
    op.timed_out = true;
    op.completed = true;
    scheduler_complete_pending_tcp_read(fid);
}

fn void scheduler_handle_wakeup_poll_error(usz fid, int status) {
    PendingTcpRead* op = &g_scheduler.pending_reads[fid];
    if (!op.active || op.completed) return;
    op.error_code = status;
    op.completed = true;
    scheduler_complete_pending_tcp_read(fid);
}

fn void scheduler_handle_wakeup_poll_readable(usz fid) {
    PendingTcpRead* op = &g_scheduler.pending_reads[fid];
    if (!op.active || op.completed) return;
    if (op.write_mode) return;
    op.readable_pending = false;

    long n = c_recv(op.fd, op.buffer, op.max_bytes, MSG_DONTWAIT);
    if (n >= 0) {
        op.nread = n;
        op.completed = true;
    } else {
        int err = 0;
        int* err_ptr = c_errno_location();
        if (err_ptr != null) err = *err_ptr;
        if (err != ERRNO_EAGAIN && err != ERRNO_EWOULDBLOCK) {
            op.error_code = err;
            op.completed = true;
        }
    }

    if (op.completed) scheduler_complete_pending_tcp_read(fid);
}

fn void scheduler_handle_wakeup_poll_writable(usz fid) {
    PendingTcpRead* op = &g_scheduler.pending_reads[fid];
    if (!op.active || op.completed) return;
    if (!op.write_mode) return;
    op.writable_pending = false;

    if (op.write_offset >= op.write_len) {
        op.nwritten = (long)op.write_offset;
        op.completed = true;
        scheduler_complete_pending_tcp_read(fid);
        return;
    }

    long n = c_send(
        op.fd,
        op.write_buffer + op.write_offset,
        op.write_len - op.write_offset,
        MSG_DONTWAIT
    );
    if (n > 0) {
        op.write_offset += (usz)n;
        op.nwritten = (long)op.write_offset;
        if (op.write_offset >= op.write_len) {
            op.completed = true;
        }
    } else if (n < 0) {
        int err = 0;
        int* err_ptr = c_errno_location();
        if (err_ptr != null) err = *err_ptr;
        if (err != ERRNO_EAGAIN && err != ERRNO_EWOULDBLOCK) {
            op.error_code = err;
            op.completed = true;
        }
    }

    if (op.completed) scheduler_complete_pending_tcp_read(fid);
}

fn void scheduler_handle_wakeup_offload_ready(usz fid, void* payload) {
    PendingOffload* off = &g_scheduler.pending_offloads[fid];
    if (!off.active || off.completed) {
        scheduler_free_offload_completion(payload);
        return;
    }

    off.completion = payload;
    off.completed = true;
    scheduler_complete_pending_offload(fid);
}

fn void scheduler_handle_wakeup_sleep_done(usz fid) {
    PendingSleep* sleep = &g_scheduler.pending_sleeps[fid];
    if (!sleep.active || sleep.completed) return;

    sleep.completed = true;
    scheduler_complete_pending_sleep(fid);
}

fn void scheduler_handle_wakeup_dns_done(usz fid) {
    PendingDnsResolve* dns = &g_scheduler.pending_dns[fid];
    if (!dns.active || !dns.completed) return;
    scheduler_complete_pending_dns(fid);
}

fn void scheduler_drain_coalesced_readable_signals() {
    for (usz fid = 0; fid < g_scheduler.fiber_count; fid++) {
        PendingTcpRead* op = &g_scheduler.pending_reads[fid];
        if (!op.active || op.completed || !op.readable_pending) continue;
        scheduler_handle_wakeup_poll_readable(fid);
    }
}

fn void scheduler_drain_coalesced_writable_signals() {
    for (usz fid = 0; fid < g_scheduler.fiber_count; fid++) {
        PendingTcpRead* op = &g_scheduler.pending_reads[fid];
        if (!op.active || op.completed || !op.writable_pending) continue;
        scheduler_handle_wakeup_poll_writable(fid);
    }
}

fn void drain_wakeups() {
    while (true) {
        ReliableWakeupNode* node = scheduler_take_reliable_wakeup_batch();
        if (node == null) break;

        while (node != null) {
            ReliableWakeupNode* next = node.next;
            scheduler_dispatch_wakeup_event(node.event);
            mem::free(node);
            node = next;
        }
    }

    scheduler_drain_coalesced_readable_signals();
    scheduler_drain_coalesced_writable_signals();
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
    return make_nil(interp);
}
