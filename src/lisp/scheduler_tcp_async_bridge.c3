module lisp;

import std::core::mem;
import std::atomic::types;
import std::io;
import std::thread;
import main;

// Async bridge used by tcp-read:
// returns non-null when it handled the call (including errors).
// null is reserved for non-fiber call sites that use different policies.
fn Value* scheduler_try_async_tcp_read(int fd, usz max_bytes, long timeout_ms, Interp* interp) {
    scheduler_init();
    if (!scheduler_is_running_fiber_context()) return null;
    Value* ctx_err = scheduler_require_fiber_root_context("tcp-read", interp);
    if (ctx_err != null) return ctx_err;

    usz fiber_id = g_scheduler.current_fiber_id;
    FiberEntry* f = &g_scheduler.fibers[fiber_id];

    PendingTcpRead* op = &g_scheduler.pending_reads[fiber_id];
    if (op.active) {
        return scheduler_raise(
            interp,
            "scheduler/tcp-read-pending-async-read-exists",
            "tcp-read: pending async read already exists for this fiber"
        );
    }

    // Fast non-blocking attempt before registering a watcher.
    Value* probe_result = scheduler_probe_tcp_read_nonblocking(fd, max_bytes, interp);
    if (probe_result != null) return probe_result;

    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return scheduler_raise(
            interp,
            "scheduler/tcp-read-libuv-unavailable",
            "tcp-read: libuv loop unavailable"
        );
    }

    usz handle_size = uv_handle_size(UV_HANDLE_POLL);
    if (handle_size == 0) {
        return scheduler_raise(
            interp,
            "scheduler/tcp-read-uv-poll-handle-unavailable",
            "tcp-read: uv poll handle unavailable"
        );
    }

    void* poll_handle = null;
    char* read_buf = null;
    Value* setup_err = scheduler_create_poll_handle(fd, max_bytes, &poll_handle, &read_buf, interp);
    if (setup_err != null) return setup_err;

    void* timer_handle = null;
    Value* timer_err = scheduler_create_timer_handle(timeout_ms, poll_handle, read_buf, &timer_handle, interp);
    if (timer_err != null) return timer_err;

    Value* start_err = scheduler_start_tcp_watchers(poll_handle, timer_handle, timeout_ms, read_buf, interp);
    if (start_err != null) return start_err;

    scheduler_init_pending_tcp_read(op, fd, max_bytes, read_buf, poll_handle, timer_handle);

    f.state = FIBER_BLOCKED;

    // Suspend current fiber until libuv callback marks it ready.
    Value*[1] yargs;
    yargs[0] = make_nil(interp);
    Value* y = prim_yield(yargs[..], null, interp);
    if (y != null && y.tag == ERROR) {
        scheduler_close_pending_tcp_read(fiber_id);
        return y;
    }

    return scheduler_consume_pending_tcp_read(fiber_id, interp);
}

// Async bridge used by tcp-write:
// returns non-null when it handled the call (including errors).
// null is reserved for non-fiber call sites that use different policies.
fn Value* scheduler_try_async_tcp_write(int fd, char[] data, Interp* interp) {
    scheduler_init();
    if (!scheduler_is_running_fiber_context()) return null;
    Value* ctx_err = scheduler_require_fiber_root_context("tcp-write", interp);
    if (ctx_err != null) return ctx_err;

    if (data.len == 0) return make_int(interp, 0);

    usz fiber_id = g_scheduler.current_fiber_id;
    FiberEntry* f = &g_scheduler.fibers[fiber_id];
    PendingTcpRead* op = &g_scheduler.pending_reads[fiber_id];
    if (op.active) {
        return scheduler_raise(
            interp,
            "scheduler/tcp-write-pending-async-io-exists",
            "tcp-write: pending async I/O already exists for this fiber"
        );
    }

    long probe_n = c_send(fd, data.ptr, data.len, MSG_DONTWAIT);
    usz written = 0;
    if (probe_n >= 0) {
        written = (usz)probe_n;
        if (written >= data.len) return make_int(interp, probe_n);
    } else {
        int probe_err = 0;
        int* err_ptr = c_errno_location();
        if (err_ptr != null) probe_err = *err_ptr;
        if (probe_err != ERRNO_EAGAIN && probe_err != ERRNO_EWOULDBLOCK) {
            return scheduler_raise(interp, "scheduler/tcp-write-send-failed", "tcp-write: send failed");
        }
    }

    // Hard-error when loop setup is unavailable; do not run sync fallback in
    // fiber async path.
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return scheduler_raise(
            interp,
            "scheduler/tcp-write-libuv-unavailable",
            "tcp-write: libuv loop unavailable"
        );
    }

    usz handle_size = uv_handle_size(UV_HANDLE_POLL);
    if (handle_size == 0) {
        return scheduler_raise(
            interp,
            "scheduler/tcp-write-uv-poll-handle-unavailable",
            "tcp-write: uv poll handle unavailable"
        );
    }

    char* write_buf = (char*)mem::malloc(data.len);
    if (write_buf == null) {
        return scheduler_raise(interp, "scheduler/tcp-write-out-of-memory", "tcp-write: out of memory");
    }
    for (usz i = 0; i < data.len; i++) write_buf[i] = data[i];

    void* poll_handle = null;
    Value* setup_err = scheduler_create_poll_handle_only(fd, &poll_handle, interp);
    if (setup_err != null) {
        mem::free(write_buf);
        return setup_err;
    }

    Value* start_err = scheduler_start_tcp_write_watcher(poll_handle, write_buf, interp);
    if (start_err != null) return start_err;

    scheduler_init_pending_tcp_write(op, fd, write_buf, data.len, written, poll_handle);
    f.state = FIBER_BLOCKED;

    Value*[1] yargs;
    yargs[0] = make_nil(interp);
    Value* y = prim_yield(yargs[..], null, interp);
    if (y != null && y.tag == ERROR) {
        scheduler_close_pending_tcp_read(fiber_id);
        return y;
    }

    return scheduler_consume_pending_tcp_read(fiber_id, interp);
}

// Async bridge reuse for UDP receive in fiber context:
// same fd readiness and wakeup path as tcp-read, but without timeout.
fn Value* scheduler_try_async_udp_recv(int fd, usz max_bytes, Interp* interp) {
    return scheduler_try_async_tcp_read(fd, max_bytes, -1, interp);
}
