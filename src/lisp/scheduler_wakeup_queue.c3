module lisp;

import std::core::mem;

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
            scheduler_handle_wakeup_offload_ready(fid, ev.payload, (uint)ev.status);
        case WAKEUP_SLEEP_DONE:
            scheduler_handle_wakeup_sleep_done(fid);
        case WAKEUP_DNS_DONE:
            scheduler_handle_wakeup_dns_done(fid);
        case WAKEUP_TCP_CONNECT_DONE:
            scheduler_handle_wakeup_tcp_connect_done(fid);
        case WAKEUP_TCP_ACCEPT_DONE:
            scheduler_handle_wakeup_tcp_accept_done(fid);
        case WAKEUP_TASK_DONE:
            scheduler_handle_wakeup_task_done(fid, (uint)ev.status);
        case WAKEUP_OS_THREAD_DONE:
            scheduler_handle_wakeup_os_thread_done(fid, (uint)ev.status);
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

fn bool scheduler_publish_offload_ready(usz fiber_id, OffloadCompletion* completion, uint wake_epoch = 0) @inline {
    WakeupEvent ev = {};
    ev.fiber_id = fiber_id;
    ev.type = WAKEUP_OFFLOAD_READY;
    ev.status = (int)wake_epoch;
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

fn bool scheduler_publish_task_done(usz fiber_id, uint wake_epoch) @inline {
    WakeupEvent ev = {};
    ev.fiber_id = fiber_id;
    ev.type = WAKEUP_TASK_DONE;
    ev.status = (int)wake_epoch;
    return scheduler_publish_reliable_wakeup(ev);
}

fn bool scheduler_publish_os_thread_done(usz fiber_id, uint wake_epoch) @inline {
    WakeupEvent ev = {};
    ev.fiber_id = fiber_id;
    ev.type = WAKEUP_OS_THREAD_DONE;
    ev.status = (int)wake_epoch;
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
