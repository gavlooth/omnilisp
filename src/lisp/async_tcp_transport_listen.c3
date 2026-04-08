module lisp;

import std::core::mem;

// ============================================================
// (tcp-listen host port [backlog]) — open listener socket (fiber-only)
// ============================================================
fn Value* prim_tcp_listen(Value*[] args, Env* env, Interp* interp) {
    Value* host_v = null;
    Value* port_v = null;
    Value* backlog_v = null;
    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        host_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) return io_raise(interp, "io/tcp-listen-expected-host-port-backlog", "tcp-listen: expected (tcp-listen host port [backlog])");
        port_v = car(packed);
        packed = cdr(packed);
        if (is_cons(packed)) backlog_v = car(packed);
    } else {
        if (args.len < 2) return io_raise(interp, "io/tcp-listen-expected-host-port-backlog", "tcp-listen: expected (tcp-listen host port [backlog])");
        host_v = args[0];
        port_v = args[1];
        if (args.len >= 3) backlog_v = args[2];
    }

    if (!is_string(host_v)) return io_raise(interp, "io/tcp-listen-host-not-string", "tcp-listen: host must be a string");
    if (!is_int(port_v)) return io_raise(interp, "io/tcp-listen-port-not-integer", "tcp-listen: port must be an integer");

    int backlog = 128;
    if (backlog_v != null) {
        if (!is_int(backlog_v)) return io_raise(interp, "io/tcp-listen-backlog-not-integer", "tcp-listen: backlog must be an integer");
        backlog = (int)backlog_v.int_val;
        if (backlog < 1) backlog = 1;
        if (backlog > 1024) backlog = 1024;
    }

    int port = (int)port_v.int_val;

    if (!scheduler_is_running_fiber_context()) {
        return tcp_raise_fiber_required(interp, "tcp-listen", "io/tcp-listen-fiber-required");
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return tcp_raise_loop_unavailable(interp, "tcp-listen", "io/tcp-listen-libuv-unavailable");
    }

    Value*[1] dns_args;
    dns_args[0] = host_v;
    Value* resolved = prim_dns_resolve(dns_args[..], null, interp);
    if (resolved == null || resolved.tag == ERROR) return resolved;
    if (!is_string(resolved)) {
        return io_raise(
            interp,
            "io/tcp-listen-resolution-invalid-result",
            "tcp-listen: DNS resolution returned invalid result"
        );
    }

    char* ip_copy = copy_to_heap_c_string(resolved.str_chars[:resolved.str_len]);
    if (ip_copy == null) {
        return io_raise(interp, "io/tcp-listen-out-of-memory", "tcp-listen: out of memory");
    }
    int uv_fd = -1;
    int status = omni_uv_tcp_listen_fd(ip_copy, port, backlog, &uv_fd);
    mem::free(ip_copy);
    if (status == 0 && uv_fd >= 0) {
        return make_tcp_listener_handle(interp, uv_fd);
    }
    return io_raise(interp, "io/tcp-listen-bind-listen-failed", "tcp-listen: bind/listen failed");
}

fn Value* tcp_accept_with_libuv(TcpHandle* listener, Interp* interp) {
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return tcp_raise_loop_unavailable(interp, "tcp-accept", "io/tcp-accept-libuv-unavailable");
    }

    usz fiber_id = g_scheduler.current_fiber_id;
    PendingTcpAccept* pending = &g_scheduler.pending_accepts[fiber_id];
    if (pending.active) {
        return io_raise(
            interp,
            "io/tcp-accept-pending-request-exists",
            "tcp-accept: pending request already exists for this fiber"
        );
    }

    void* server_handle = null;
    void* client_handle = null;
    bool borrowed_server = false;
    int start_status = 0;
    if (listener.transport == TCP_TRANSPORT_PIPE) {
        if (listener.uv_listener_handle == null) {
            return io_raise(interp, "io/tcp-accept-invalid-listener-handle", "tcp-accept: invalid or closed listener handle");
        }
        server_handle = listener.uv_listener_handle;
        borrowed_server = true;
        start_status = omni_uv_pipe_accept_prepare(
            g_scheduler.uv_loop,
            server_handle,
            &scheduler_uv_tcp_accept_cb,
            &client_handle
        );
    } else {
        start_status = omni_uv_tcp_accept_start(
            g_scheduler.uv_loop,
            listener.fd,
            &scheduler_uv_tcp_accept_cb,
            &server_handle,
            &client_handle
        );
    }
    if (start_status != 0 || server_handle == null || client_handle == null) {
        return io_raise(interp, "io/tcp-accept-async-init-failed", "tcp-accept: async accept init failed");
    }

    if (borrowed_server) {
        int immediate_fd = -1;
        int immediate_status = omni_uv_tcp_accept_detach_fd(server_handle, client_handle, &immediate_fd);
        if (immediate_status == 0 && immediate_fd >= 0) {
            (void)omni_uv_pipe_listener_set_noop(server_handle);
            omni_uv_tcp_close_and_free(client_handle);
            return make_tcp_stream_handle(interp, immediate_fd, listener.transport);
        }
    }

    pending.active = true;
    pending.completed = false;
    pending.status = 0;
    pending.borrowed_server_handle = borrowed_server;
    pending.has_accepted_fd = false;
    pending.accepted_fd = -1;
    pending.server_handle = server_handle;
    pending.client_handle = client_handle;

    g_scheduler.fibers[fiber_id].state = FIBER_BLOCKED;

    Value*[1] yargs;
    yargs[0] = make_nil(interp);
    Value* y = prim_yield(yargs[..], null, interp);
    if (y != null && y.tag == ERROR) return y;

    if (!pending.active || !pending.completed) {
        return io_raise(
            interp,
            "io/tcp-accept-resumed-before-completion",
            "tcp-accept: resumed before accept completion"
        );
    }

    int status = pending.status;
    int accepted_fd = pending.has_accepted_fd ? pending.accepted_fd : -1;
    void* done_server = pending.server_handle;
    void* done_client = pending.client_handle;
    bool borrowed_done_server = pending.borrowed_server_handle;
    TcpTransport accepted_transport = listener.transport;
    g_scheduler.pending_accepts[fiber_id] = {};

    if (done_server != null && !borrowed_done_server) omni_uv_tcp_close_and_free(done_server);
    if (done_client != null) omni_uv_tcp_close_and_free(done_client);

    if (status != 0 || accepted_fd < 0) {
        return io_raise(interp, "io/tcp-accept-failed", "tcp-accept: accept failed");
    }
    return make_tcp_stream_handle(interp, accepted_fd, accepted_transport);
}

// ============================================================
// (tcp-accept listener-handle) — accept one connection
// ============================================================
fn Value* prim_tcp_accept(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return io_raise(interp, "io/tcp-accept-expected-listener-handle", "tcp-accept: expected (tcp-accept listener-handle)");
    TcpHandle* listener = get_tcp_handle(args[0]);
    if (listener == null || !listener.connected || listener.mode != TCP_LISTENER) {
        return io_raise(interp, "io/tcp-accept-invalid-listener-handle", "tcp-accept: invalid or closed listener handle");
    }

    if (!scheduler_is_running_fiber_context()) {
        return tcp_raise_fiber_required(interp, "tcp-accept", "io/tcp-accept-fiber-required");
    }
    return tcp_accept_with_libuv(listener, interp);
}
