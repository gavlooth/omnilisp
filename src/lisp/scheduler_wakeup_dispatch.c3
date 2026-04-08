module lisp;

import std::core::mem;

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

fn void scheduler_handle_wakeup_offload_ready(usz fid, void* payload, uint wake_epoch) {
    PendingOffload* off = &g_scheduler.pending_offloads[fid];
    if (!off.active || off.completed) {
        scheduler_free_offload_completion(payload);
        return;
    }
    if (off.wake_epoch != 0 && wake_epoch != off.wake_epoch) {
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

fn void scheduler_handle_wakeup_task_done(usz fid, uint wake_epoch) {
    if (wake_epoch == 0) return;
    if (g_scheduler.fiber_join_wait_token[fid] != wake_epoch) return;
    g_scheduler.fiber_join_wait_token[fid] = 0;
    if (g_scheduler.fibers[fid].state == FIBER_BLOCKED) g_scheduler.fibers[fid].state = FIBER_READY;
}

fn void scheduler_handle_wakeup_os_thread_done(usz fid, uint wake_epoch) {
    if (wake_epoch == 0) return;
    if (g_scheduler.fiber_join_wait_token[fid] != wake_epoch) return;
    g_scheduler.fiber_join_wait_token[fid] = 0;
    if (g_scheduler.fibers[fid].state == FIBER_BLOCKED) g_scheduler.fibers[fid].state = FIBER_READY;
}

fn void scheduler_drain_latched_join_wakeups() {
    if (!g_scheduler.thread_task_mu_ready) return;

    uint[MAX_FIBERS] task_epochs;
    uint[MAX_FIBERS] os_thread_epochs;
    usz count = g_scheduler.fiber_count;
    if (count > MAX_FIBERS) count = MAX_FIBERS;

    g_scheduler.thread_task_mu.lock();
    for (usz fid = 0; fid < count; fid++) {
        task_epochs[fid] = g_scheduler.latched_task_done_wake_epoch[fid];
        os_thread_epochs[fid] = g_scheduler.latched_os_thread_done_wake_epoch[fid];
        g_scheduler.latched_task_done_wake_epoch[fid] = 0;
        g_scheduler.latched_os_thread_done_wake_epoch[fid] = 0;
    }
    g_scheduler.thread_task_mu.unlock();

    for (usz fid = 0; fid < count; fid++) {
        if (task_epochs[fid] != 0) {
            scheduler_handle_wakeup_task_done(fid, task_epochs[fid]);
        }
        if (os_thread_epochs[fid] != 0) {
            scheduler_handle_wakeup_os_thread_done(fid, os_thread_epochs[fid]);
        }
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

    scheduler_drain_latched_join_wakeups();
    scheduler_drain_coalesced_readable_signals();
    scheduler_drain_coalesced_writable_signals();
}
