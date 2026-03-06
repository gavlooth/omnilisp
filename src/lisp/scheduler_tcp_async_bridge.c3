module lisp;

import std::core::mem;
import std::atomic::types;
import std::io;
import std::thread;
import main;

fn Value* scheduler_probe_tcp_read_nonblocking(int fd, usz max_bytes, Interp* interp) {
    char* probe_buf = (char*)mem::malloc(max_bytes);
    if (probe_buf == null) return scheduler_raise(interp, "scheduler/tcp-read-out-of-memory", "tcp-read: out of memory");

    long probe_n = c_recv(fd, probe_buf, max_bytes, MSG_DONTWAIT);
    if (probe_n >= 0) {
        Value* out = probe_n == 0 ? make_string(interp, "") : make_string(interp, probe_buf[:(usz)probe_n]);
        mem::free(probe_buf);
        return out;
    }

    int probe_err = 0;
    int* err_ptr = c_errno_location();
    if (err_ptr != null) probe_err = *err_ptr;
    mem::free(probe_buf);

    if (probe_err != ERRNO_EAGAIN && probe_err != ERRNO_EWOULDBLOCK) {
        return scheduler_raise(interp, "scheduler/tcp-read-recv-failed", "tcp-read: recv failed");
    }
    return null;
}

fn void scheduler_init_pending_tcp_read(PendingTcpRead* op, int fd, usz max_bytes, char* read_buf, void* poll_handle, void* timer_handle) {
    op.active = true;
    op.completed = false;
    op.consumed = false;
    op.timed_out = false;
    op.readable_pending = false;
    op.fd = fd;
    op.max_bytes = max_bytes;
    op.buffer = read_buf;
    op.nread = -1;
    op.error_code = 0;
    op.poll_handle = poll_handle;
    op.timer_handle = timer_handle;
}

fn void scheduler_drain_loop_nowait() @inline {
    if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
        (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
    }
}

fn void scheduler_close_handles_and_drain(void* poll_handle, void* timer_handle) {
    if (poll_handle != null) {
        uv_close(poll_handle, &scheduler_free_handle_cb);
    }
    if (timer_handle != null) {
        uv_close(timer_handle, &scheduler_free_handle_cb);
    }
    scheduler_drain_loop_nowait();
}

fn Value* scheduler_create_poll_handle(int fd, usz max_bytes, void** poll_handle_out, char** read_buf_out, Interp* interp) {
    void* poll_handle = mem::malloc(uv_handle_size(UV_HANDLE_POLL));
    if (poll_handle == null) return scheduler_raise(interp, "scheduler/tcp-read-out-of-memory", "tcp-read: out of memory");

    char* read_buf = (char*)mem::malloc(max_bytes);
    if (read_buf == null) {
        mem::free(poll_handle);
        return scheduler_raise(interp, "scheduler/tcp-read-out-of-memory", "tcp-read: out of memory");
    }

    if (uv_poll_init(g_scheduler.uv_loop, poll_handle, fd) != 0) {
        mem::free(read_buf);
        mem::free(poll_handle);
        return scheduler_raise(interp, "scheduler/tcp-read-uv-poll-init-failed", "tcp-read: uv_poll_init failed");
    }

    *poll_handle_out = poll_handle;
    *read_buf_out = read_buf;
    return null;
}

fn Value* scheduler_create_timer_handle(long timeout_ms, void* poll_handle, char* read_buf, void** timer_handle_out, Interp* interp) {
    *timer_handle_out = null;
    if (timeout_ms < 0) return null;

    usz timer_size = uv_handle_size(UV_HANDLE_TIMER);
    if (timer_size == 0) {
        scheduler_close_handles_and_drain(poll_handle, null);
        mem::free(read_buf);
        return scheduler_raise(
            interp,
            "scheduler/tcp-read-uv-timer-handle-unavailable",
            "tcp-read: uv timer handle unavailable"
        );
    }

    void* timer_handle = mem::malloc(timer_size);
    if (timer_handle == null) {
        scheduler_close_handles_and_drain(poll_handle, null);
        mem::free(read_buf);
        return scheduler_raise(interp, "scheduler/tcp-read-out-of-memory", "tcp-read: out of memory");
    }

    if (uv_timer_init(g_scheduler.uv_loop, timer_handle) != 0) {
        mem::free(timer_handle);
        scheduler_close_handles_and_drain(poll_handle, null);
        mem::free(read_buf);
        return scheduler_raise(interp, "scheduler/tcp-read-uv-timer-init-failed", "tcp-read: uv_timer_init failed");
    }

    *timer_handle_out = timer_handle;
    return null;
}

fn Value* scheduler_start_tcp_watchers(void* poll_handle, void* timer_handle, long timeout_ms, char* read_buf, Interp* interp) {
    if (uv_poll_start(poll_handle, UV_READABLE, &scheduler_uv_poll_cb) != 0) {
        scheduler_close_handles_and_drain(poll_handle, timer_handle);
        mem::free(read_buf);
        return scheduler_raise(interp, "scheduler/tcp-read-uv-poll-start-failed", "tcp-read: uv_poll_start failed");
    }

    if (timer_handle != null && uv_timer_start(timer_handle, &scheduler_uv_timer_cb, (ulong)timeout_ms, 0) != 0) {
        (void)uv_poll_stop(poll_handle);
        scheduler_close_handles_and_drain(poll_handle, timer_handle);
        mem::free(read_buf);
        return scheduler_raise(interp, "scheduler/tcp-read-uv-timer-start-failed", "tcp-read: uv_timer_start failed");
    }

    return null;
}

// Async bridge used by tcp-read fast-path:
// returns non-null when it handled the call (including errors);
// null means "fall back to blocking path".
fn Value* scheduler_try_async_tcp_read(int fd, usz max_bytes, long timeout_ms, Interp* interp) {
    scheduler_init();
    if (!scheduler_is_running_fiber_context()) return null;

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

    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) return null;

    usz handle_size = uv_handle_size(UV_HANDLE_POLL);
    if (handle_size == 0) return null;

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
    if (y != null && y.tag == ERROR) return y;

    return scheduler_consume_pending_tcp_read(fiber_id, interp);
}
