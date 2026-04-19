module lisp;

import std::io;

// ============================================================
// (tcp-write handle data) — blocking write, returns bytes written
// ============================================================

fn Value* prim_tcp_write(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 2) return io_raise(interp, "io/tcp-write-expected-handle-data", "tcp-write: expected (tcp-write handle data)");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[1])) return io_raise(interp, "io/tcp-write-data-not-string", "tcp-write: data must be a string");
    if (!scheduler_is_running_fiber_context()) {
        return tcp_raise_fiber_required(interp, "tcp-write", "io/tcp-write-fiber-required");
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return tcp_raise_loop_unavailable(interp, "tcp-write", "io/tcp-write-libuv-unavailable");
    }

    TcpHandle* th = get_tcp_handle(args[0]);
    // fault: lisp::TYPE_MISMATCH
    if (th == null || !th.connected || th.mode != TCP_STREAM) {
        return io_raise(interp, "io/tcp-write-invalid-stream-handle", "tcp-write: invalid or closed stream handle");
    }

    char[] data = args[1].str_chars[:args[1].str_len];
    // Async bridge: in scheduler-managed fiber context, use libuv writable
    // wakeups so writes do not block the runtime thread.
    Value* async_result = scheduler_try_async_tcp_write(th.fd, data, interp);
    if (async_result != null) return async_result;
    return io_raise(
        interp,
        "io/tcp-write-async-bridge-unavailable",
        "tcp-write: async bridge unavailable"
    );
}

// ============================================================
// (tcp-read handle [max-bytes]) — blocking read, returns string
// ============================================================

fn Value* prim_tcp_read(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return io_raise(interp, "io/tcp-read-expected-handle-max-bytes", "tcp-read: expected (tcp-read handle [max-bytes])");
    if (!scheduler_is_running_fiber_context()) {
        return tcp_raise_fiber_required(interp, "tcp-read", "io/tcp-read-fiber-required");
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return tcp_raise_loop_unavailable(interp, "tcp-read", "io/tcp-read-libuv-unavailable");
    }

    TcpHandle* th = get_tcp_handle(args[0]);
    // fault: lisp::TYPE_MISMATCH
    if (th == null || !th.connected || th.mode != TCP_STREAM) {
        return io_raise(interp, "io/tcp-read-invalid-stream-handle", "tcp-read: invalid or closed stream handle");
    }

    usz max_bytes = 0;
    long timeout_ms = -1;
    if (Value* opt_err = parse_tcp_read_options(args, &max_bytes, &timeout_ms, interp)) return opt_err;

    // Async bridge: register a non-blocking read with libuv and suspend this
    // fiber until data is ready.
    Value* async_result = scheduler_try_async_tcp_read(th.fd, max_bytes, timeout_ms, interp);
    if (async_result != null) return async_result;
    return io_raise(
        interp,
        "io/tcp-read-async-bridge-unavailable",
        "tcp-read: async bridge unavailable"
    );
}

// ============================================================
// (tcp-read-timeout handle timeout-ms) — read with timeout
// ============================================================

fn Value* prim_tcp_read_timeout(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) {
        return io_raise(interp, "io/tcp-read-timeout-expected-handle-timeout", "tcp-read-timeout: expected (tcp-read-timeout handle timeout-ms)");
    }
    if (!is_int(args[1])) {
        return io_raise(interp, "io/tcp-read-timeout-timeout-not-integer", "tcp-read-timeout: timeout-ms must be an integer");
    }

    Value*[3] read_args;
    read_args[0] = args[0];             // handle
    read_args[1] = make_int(interp, 4096); // default max-bytes
    read_args[2] = args[1];             // timeout-ms
    return prim_tcp_read(read_args[..], env, interp);
}

// ============================================================
// (tcp-close handle) — close connection (fiber-only)
// ============================================================

fn Value* prim_tcp_close(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return io_raise(interp, "io/tcp-close-expected-handle", "tcp-close: expected (tcp-close handle)");

    TcpHandle* th = get_tcp_handle(args[0]);
    // fault: lisp::TYPE_MISMATCH
    if (th == null) return io_raise(interp, "io/tcp-close-invalid-handle", "tcp-close: invalid handle");
    if (!scheduler_is_running_fiber_context()) {
        return tcp_raise_fiber_required(interp, "tcp-close", "io/tcp-close-fiber-required");
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return tcp_raise_loop_unavailable(interp, "tcp-close", "io/tcp-close-libuv-unavailable");
    }

    if (th.connected) {
        if (th.uv_listener_handle != null) {
            omni_uv_tcp_close_and_free(th.uv_listener_handle);
            th.uv_listener_handle = null;
        }
        int close_status = omni_uv_tcp_close_fd(th.fd);
        if (close_status != 0) {
            close_status = c_close_fd(th.fd);
            (void)close_status;
        }
        th.connected = false;
        th.fd = -1;
    }

    return make_void(interp);
}
