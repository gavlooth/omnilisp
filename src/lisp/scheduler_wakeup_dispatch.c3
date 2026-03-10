module lisp;

import std::core::mem;
import std::atomic::types;
import std::io;
import std::thread;
import main;

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
        case WAKEUP_TCP_CONNECT_DONE:
            scheduler_handle_wakeup_tcp_connect_done(fid);
        case WAKEUP_TCP_ACCEPT_DONE:
            scheduler_handle_wakeup_tcp_accept_done(fid);
        case WAKEUP_TASK_DONE:
            scheduler_handle_wakeup_task_done(fid);
        case WAKEUP_OS_THREAD_DONE:
            scheduler_handle_wakeup_os_thread_done(fid);
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

fn bool scheduler_publish_tcp_connect_done(usz fiber_id) @inline {
    WakeupEvent ev = {};
    ev.fiber_id = fiber_id;
    ev.type = WAKEUP_TCP_CONNECT_DONE;
    return scheduler_publish_reliable_wakeup(ev);
}

fn bool scheduler_publish_tcp_accept_done(usz fiber_id) @inline {
    WakeupEvent ev = {};
    ev.fiber_id = fiber_id;
    ev.type = WAKEUP_TCP_ACCEPT_DONE;
    return scheduler_publish_reliable_wakeup(ev);
}

fn bool scheduler_publish_task_done(usz fiber_id) @inline {
    WakeupEvent ev = {};
    ev.fiber_id = fiber_id;
    ev.type = WAKEUP_TASK_DONE;
    return scheduler_publish_reliable_wakeup(ev);
}

fn bool scheduler_publish_os_thread_done(usz fiber_id) @inline {
    WakeupEvent ev = {};
    ev.fiber_id = fiber_id;
    ev.type = WAKEUP_OS_THREAD_DONE;
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

fn void scheduler_uv_tcp_connect_cb(void* req, int status) {
    isz idx = scheduler_find_pending_tcp_connect_by_req(req);
    if (idx < 0) {
        void* orphan_handle = omni_uv_req_get_data(req);
        if (orphan_handle != null) {
            omni_uv_tcp_close_and_free(orphan_handle);
        }
        omni_uv_tcp_connect_req_free(req);
        return;
    }

    PendingTcpConnect* pending = &g_scheduler.pending_connects[(usz)idx];
    pending.status = status;
    pending.completed = true;

    if (!pending.active || pending.req_handle == null || pending.tcp_handle == null) {
        if (pending.req_handle != null) {
            omni_uv_tcp_connect_req_free(pending.req_handle);
        }
        if (pending.tcp_handle != null) {
            omni_uv_tcp_close_and_free(pending.tcp_handle);
        }
        g_scheduler.pending_connects[(usz)idx] = {};
        return;
    }

    if (!scheduler_publish_tcp_connect_done((usz)idx)) {
        scheduler_complete_pending_tcp_connect((usz)idx);
    }
}

fn void scheduler_uv_tcp_accept_cb(void* server_handle, int status) {
    isz idx = scheduler_find_pending_tcp_accept_by_server(server_handle);
    if (idx < 0) {
        return;
    }

    PendingTcpAccept* pending = &g_scheduler.pending_accepts[(usz)idx];
    if (!pending.active ||
        pending.completed ||
        pending.server_handle == null ||
        pending.client_handle == null) {
        scheduler_close_pending_tcp_accept((usz)idx);
        return;
    }

    int accepted_fd = -1;
    int accept_status = status;
    if (status >= 0) {
        accept_status = omni_uv_tcp_accept_detach_fd(pending.server_handle, pending.client_handle, &accepted_fd);
    }

    void* done_server = pending.server_handle;
    void* done_client = pending.client_handle;
    bool borrowed_done_server = pending.borrowed_server_handle;
    pending.server_handle = null;
    pending.client_handle = null;
    if (done_server != null && borrowed_done_server) {
        (void)omni_uv_pipe_listener_set_noop(done_server);
    }
    if (done_server != null && !borrowed_done_server) {
        omni_uv_tcp_close_and_free(done_server);
    }
    if (done_client != null) {
        omni_uv_tcp_close_and_free(done_client);
    }

    pending.status = accept_status;
    pending.accepted_fd = accepted_fd;
    pending.has_accepted_fd = accepted_fd >= 0;
    pending.completed = true;

    if (!scheduler_publish_tcp_accept_done((usz)idx)) {
        scheduler_complete_pending_tcp_accept((usz)idx);
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

fn void scheduler_handle_wakeup_tcp_connect_done(usz fid) {
    PendingTcpConnect* pending = &g_scheduler.pending_connects[fid];
    if (!pending.active || !pending.completed) return;
    scheduler_complete_pending_tcp_connect(fid);
}

fn void scheduler_handle_wakeup_tcp_accept_done(usz fid) {
    PendingTcpAccept* pending = &g_scheduler.pending_accepts[fid];
    if (!pending.active || !pending.completed) return;
    scheduler_complete_pending_tcp_accept(fid);
}

fn void scheduler_handle_wakeup_task_done(usz fid) {
    if (g_scheduler.fibers[fid].state == FIBER_BLOCKED) {
        g_scheduler.fibers[fid].state = FIBER_READY;
    }
}

fn void scheduler_handle_wakeup_os_thread_done(usz fid) {
    if (g_scheduler.fibers[fid].state == FIBER_BLOCKED) {
        g_scheduler.fibers[fid].state = FIBER_READY;
    }
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
