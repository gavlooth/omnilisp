module lisp;

import std::core::mem;
import std::io;
import main;

fn Value* prim_udp_socket(Value*[] args, Env* env, Interp* interp) {
    // Effect fast-path for arity-0 primitives may pass a single nil payload.
    if (args.len > 1 || (args.len == 1 && !is_nil(args[0]))) {
        return io_raise(interp, "io/udp-socket-expected-no-args", "udp-socket: expected (udp-socket)");
    }

    int fd = c_socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (fd < 0) {
        return io_raise(interp, "io/udp-socket-create-failed", "udp-socket: socket failed");
    }
    return make_udp_handle(interp, fd);
}

// (udp-bind handle host port) — bind UDP socket to local address
fn Value* prim_udp_bind(Value*[] args, Env* env, Interp* interp) {
    Value* handle_v = null;
    Value* host_v = null;
    Value* port_v = null;
    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        handle_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) {
            return io_raise(interp, "io/udp-bind-expected-handle-host-port", "udp-bind: expected (udp-bind handle host port)");
        }
        host_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) {
            return io_raise(interp, "io/udp-bind-expected-handle-host-port", "udp-bind: expected (udp-bind handle host port)");
        }
        port_v = car(packed);
    } else if (args.len >= 3) {
        handle_v = args[0];
        host_v = args[1];
        port_v = args[2];
    } else {
        return io_raise(interp, "io/udp-bind-expected-handle-host-port", "udp-bind: expected (udp-bind handle host port)");
    }

    if (!is_string(host_v)) {
        return io_raise(interp, "io/udp-bind-host-not-string", "udp-bind: host must be a string");
    }
    if (!is_int(port_v)) {
        return io_raise(interp, "io/udp-bind-port-not-integer", "udp-bind: port must be an integer");
    }

    UdpHandle* uh = get_udp_handle(handle_v);
    if (uh == null || !uh.open || uh.fd < 0) {
        return io_raise(interp, "io/udp-bind-invalid-handle", "udp-bind: invalid or closed handle");
    }

    SockaddrIn addr = {};
    char[] host = host_v.str_chars[:host_v.str_len];
    int port = (int)port_v.int_val;
    if (!fill_sockaddr_ipv4(host, port, &addr)) {
        return io_raise(interp, "io/udp-bind-invalid-address", "udp-bind: invalid host or port");
    }

    int reuse = 1;
    (void)c_setsockopt(uh.fd, SOL_SOCKET, SO_REUSEADDR, &reuse, 4);

    if (c_bind(uh.fd, &addr, SockaddrIn.sizeof) < 0) {
        return io_raise(interp, "io/udp-bind-failed", "udp-bind: bind failed");
    }
    return make_nil(interp);
}

// (udp-send handle host port data) — send one datagram
fn Value* prim_udp_send(Value*[] args, Env* env, Interp* interp) {
    Value* handle_v = null;
    Value* host_v = null;
    Value* port_v = null;
    Value* data_v = null;
    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        handle_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) {
            return io_raise(interp, "io/udp-send-expected-handle-host-port-data", "udp-send: expected (udp-send handle host port data)");
        }
        host_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) {
            return io_raise(interp, "io/udp-send-expected-handle-host-port-data", "udp-send: expected (udp-send handle host port data)");
        }
        port_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) {
            return io_raise(interp, "io/udp-send-expected-handle-host-port-data", "udp-send: expected (udp-send handle host port data)");
        }
        data_v = car(packed);
    } else if (args.len >= 4) {
        handle_v = args[0];
        host_v = args[1];
        port_v = args[2];
        data_v = args[3];
    } else {
        return io_raise(interp, "io/udp-send-expected-handle-host-port-data", "udp-send: expected (udp-send handle host port data)");
    }

    if (!is_string(host_v)) {
        return io_raise(interp, "io/udp-send-host-not-string", "udp-send: host must be a string");
    }
    if (!is_int(port_v)) {
        return io_raise(interp, "io/udp-send-port-not-integer", "udp-send: port must be an integer");
    }
    if (!is_string(data_v)) {
        return io_raise(interp, "io/udp-send-data-not-string", "udp-send: data must be a string");
    }

    UdpHandle* uh = get_udp_handle(handle_v);
    if (uh == null || !uh.open || uh.fd < 0) {
        return io_raise(interp, "io/udp-send-invalid-handle", "udp-send: invalid or closed handle");
    }

    SockaddrIn addr = {};
    char[] host = host_v.str_chars[:host_v.str_len];
    int port = (int)port_v.int_val;
    if (!fill_sockaddr_ipv4(host, port, &addr)) {
        return io_raise(interp, "io/udp-send-invalid-address", "udp-send: invalid host or port");
    }

    char[] data = data_v.str_chars[:data_v.str_len];
    long sent = c_sendto(uh.fd, data.ptr, data.len, 0, &addr, SockaddrIn.sizeof);
    if (sent < 0) {
        return io_raise(interp, "io/udp-send-failed", "udp-send: send failed");
    }
    return make_int(interp, sent);
}

// (udp-recv handle [max-bytes]) — receive one datagram payload as string
fn Value* prim_udp_recv(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) {
        return io_raise(interp, "io/udp-recv-expected-handle", "udp-recv: expected (udp-recv handle [max-bytes])");
    }

    UdpHandle* uh = get_udp_handle(args[0]);
    if (uh == null || !uh.open || uh.fd < 0) {
        return io_raise(interp, "io/udp-recv-invalid-handle", "udp-recv: invalid or closed handle");
    }

    usz max_bytes = 4096;
    if (args.len >= 2) {
        if (!is_int(args[1])) {
            return io_raise(interp, "io/udp-recv-max-bytes-not-integer", "udp-recv: max-bytes must be an integer");
        }
        long n = args[1].int_val;
        if (n <= 0) {
            return io_raise(interp, "io/udp-recv-max-bytes-invalid", "udp-recv: max-bytes must be > 0");
        }
        max_bytes = (usz)n;
        if (max_bytes > 65536) max_bytes = 65536;
    }

    // Async bridge: in scheduler-managed fiber context, use libuv readiness
    // and wakeup paths instead of blocking recv.
    Value* async_result = scheduler_try_async_udp_recv(uh.fd, max_bytes, interp);
    if (async_result != null) return async_result;

    char* buf = (char*)mem::malloc(max_bytes);
    if (buf == null) {
        return io_raise(interp, "io/udp-recv-out-of-memory", "udp-recv: out of memory");
    }
    defer mem::free(buf);

    long received = c_recv(uh.fd, buf, max_bytes, 0);
    if (received < 0) {
        return io_raise(interp, "io/udp-recv-failed", "udp-recv: recv failed");
    }
    if (received == 0) {
        return make_string(interp, "");
    }
    return make_string(interp, buf[:(usz)received]);
}

// (udp-close handle) — close UDP handle
fn Value* prim_udp_close(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) {
        return io_raise(interp, "io/udp-close-expected-handle", "udp-close: expected (udp-close handle)");
    }

    UdpHandle* uh = get_udp_handle(args[0]);
    if (uh == null) {
        return io_raise(interp, "io/udp-close-invalid-handle", "udp-close: invalid handle");
    }

    if (uh.open && uh.fd >= 0) {
        c_close_fd(uh.fd);
        uh.open = false;
        uh.fd = -1;
    }
    return make_nil(interp);
}

// ============================================================
// Unix pipe/socket primitives
// ============================================================

// (pipe-connect path) — connect to a Unix domain socket path
fn Value* prim_pipe_connect(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return io_raise(interp, "io/pipe-connect-expected-path", "pipe-connect: expected (pipe-connect path)");
    }
    if (!scheduler_is_running_fiber_context()) {
        return tcp_raise_fiber_required(interp, "pipe-connect", "io/pipe-connect-fiber-required");
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return tcp_raise_loop_unavailable(interp, "pipe-connect", "io/pipe-connect-libuv-unavailable");
    }

    usz fiber_id = g_scheduler.current_fiber_id;
    PendingTcpConnect* pending = &g_scheduler.pending_connects[fiber_id];
    if (pending.active) {
        return io_raise(
            interp,
            "io/pipe-connect-pending-request-exists",
            "pipe-connect: pending request already exists for this fiber"
        );
    }

    char[] path = args[0].str_chars[:args[0].str_len];
    char* path_heap = copy_to_heap_c_string(path);
    if (path_heap == null) {
        return io_raise(interp, "io/pipe-connect-out-of-memory", "pipe-connect: out of memory");
    }

    void* req_handle = null;
    void* pipe_handle = null;
    int start_status = omni_uv_pipe_connect_start(
        g_scheduler.uv_loop,
        path_heap,
        &scheduler_uv_tcp_connect_cb,
        &req_handle,
        &pipe_handle
    );
    mem::free(path_heap);
    if (start_status != 0 || req_handle == null || pipe_handle == null) {
        return io_raise(interp, "io/pipe-connect-async-init-failed", "pipe-connect: async connect init failed");
    }

    pending.active = true;
    pending.completed = false;
    pending.status = 0;
    pending.req_handle = req_handle;
    pending.tcp_handle = pipe_handle;

    g_scheduler.fibers[fiber_id].state = FIBER_BLOCKED;

    Value*[1] yargs;
    yargs[0] = make_nil(interp);
    Value* y = prim_yield(yargs[..], null, interp);
    if (y != null && y.tag == ERROR) return y;

    if (!pending.active || !pending.completed || pending.req_handle == null || pending.tcp_handle == null) {
        return io_raise(
            interp,
            "io/pipe-connect-resumed-before-completion",
            "pipe-connect: resumed before connect completion"
        );
    }

    int status = pending.status;
    void* done_req = pending.req_handle;
    void* done_pipe = pending.tcp_handle;
    g_scheduler.pending_connects[fiber_id] = {};

    if (status != 0) {
        omni_uv_tcp_connect_req_free(done_req);
        omni_uv_tcp_close_and_free(done_pipe);
        return io_raise(interp, "io/pipe-connect-failed", "pipe-connect: connect failed");
    }

    int fd = -1;
    int fd_status = omni_uv_tcp_connect_detach_fd(done_pipe, &fd);
    omni_uv_tcp_connect_req_free(done_req);
    omni_uv_tcp_close_and_free(done_pipe);
    if (fd_status != 0 || fd < 0) {
        return io_raise(interp, "io/pipe-connect-fd-detach-failed", "pipe-connect: failed to detach socket");
    }

    return make_pipe_handle(interp, fd);
}

// (pipe-listen path) — create a Unix domain listener socket at path
fn Value* prim_pipe_listen(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return io_raise(interp, "io/pipe-listen-expected-path", "pipe-listen: expected (pipe-listen path)");
    }
    if (!scheduler_is_running_fiber_context()) {
        return tcp_raise_fiber_required(interp, "pipe-listen", "io/pipe-listen-fiber-required");
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return tcp_raise_loop_unavailable(interp, "pipe-listen", "io/pipe-listen-libuv-unavailable");
    }

    char[] path = args[0].str_chars[:args[0].str_len];
    char* path_heap = copy_to_heap_c_string(path);
    if (path_heap == null) {
        return io_raise(interp, "io/pipe-listen-out-of-memory", "pipe-listen: out of memory");
    }
    defer mem::free(path_heap);

    int fd = -1;
    void* listener_handle = null;
    int uv_status = omni_uv_pipe_listen_start(g_scheduler.uv_loop, path_heap, 128, &listener_handle, &fd);
    if (uv_status < 0 || fd < 0) fd = -1;
    if (fd < 0) {
        return io_raise(interp, "io/pipe-listen-failed", "pipe-listen: listen failed");
    }
    return make_pipe_listener_handle(interp, fd, listener_handle);
}

