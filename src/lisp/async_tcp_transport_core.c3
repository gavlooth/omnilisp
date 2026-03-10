module lisp;

import std::core::mem;
import std::io;

fn bool is_localhost_name(char[] host) @inline {
    return host.len == 9 &&
           host[0] == 'l' &&
           host[1] == 'o' &&
           host[2] == 'c' &&
           host[3] == 'a' &&
           host[4] == 'l' &&
           host[5] == 'h' &&
           host[6] == 'o' &&
           host[7] == 's' &&
           host[8] == 't';
}

fn bool fill_sockaddr_ipv4(char[] host, int port, SockaddrIn* out_addr) {
    if (out_addr == null) return false;
    if (port < 0 || port > 65535) return false;

    char[64] host_buf;
    copy_to_c_buffer(&host_buf, host_buf.len, host);

    out_addr.sin_family = (ushort)AF_INET;
    out_addr.sin_port = htons((ushort)port);
    if (is_localhost_name(host)) {
        out_addr.sin_addr = c_inet_addr("127.0.0.1");
    } else {
        out_addr.sin_addr = c_inet_addr(&host_buf);
    }
    return out_addr.sin_addr != 0xFFFFFFFFu;
}

fn bool resolve_host_and_port(char[] host, int port, void** out_result) {
    char[256] host_buf;  // buffer: C-interop (keep) — null-terminated for c_getaddrinfo
    char[8] port_buf;    // buffer: C-interop (keep) — null-terminated for c_getaddrinfo
    copy_to_c_buffer(&host_buf, host_buf.len, host);
    int plen = io::bprintf(&port_buf, "%d", port)!!.len;
    if (plen < 0) return false;
    if ((usz)plen >= port_buf.len) {
        port_buf[port_buf.len - 1] = 0;
    } else {
        port_buf[(usz)plen] = 0;
    }
    return c_getaddrinfo(&host_buf, &port_buf, null, out_result) == 0 && *out_result != null;
}

fn int connect_fd_from_addrinfo(void* result) {
    // struct addrinfo offsets (x86_64 glibc): flags=0, family=4, socktype=8, protocol=12, addrlen=16, addr=24
    int ai_family = *((int*)((char*)result + 4));
    int ai_socktype = *((int*)((char*)result + 8));
    int ai_protocol = *((int*)((char*)result + 12));
    uint ai_addrlen = *((uint*)((char*)result + 16));
    void* ai_addr = *((void**)((char*)result + 24));

    int fd = c_socket(ai_family, ai_socktype, ai_protocol);
    if (fd < 0) return -1;
    if (c_connect(fd, ai_addr, ai_addrlen) < 0) {
        c_close_fd(fd);
        return -1;
    }
    return fd;
}

fn int listen_fd_from_host(char[] host, int port, int backlog) {
    int fd = c_socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (fd < 0) return -1;

    int reuse = 1;
    (void)c_setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &reuse, 4);

    SockaddrIn addr = {};
    if (!fill_sockaddr_ipv4(host, port, &addr)) {
        c_close_fd(fd);
        return -1;
    }

    if (c_bind(fd, &addr, SockaddrIn.sizeof) < 0) {
        c_close_fd(fd);
        return -1;
    }
    if (c_listen(fd, backlog) < 0) {
        c_close_fd(fd);
        return -1;
    }
    return fd;
}

fn int tcp_accept_fd(int listener_fd) {
    return c_accept(listener_fd, null, null);
}

fn void parse_tcp_read_options(Value*[] args, usz* max_bytes, long* timeout_ms) {
    *max_bytes = 4096;
    if (args.len >= 2 && is_int(args[1])) {
        *max_bytes = (usz)args[1].int_val;
        if (*max_bytes > 65536) *max_bytes = 65536;
    }
    *timeout_ms = -1;
    if (args.len >= 3 && is_int(args[2])) {
        *timeout_ms = args[2].int_val;
        if (*timeout_ms < 0) *timeout_ms = -1;
    }
}

fn Value* dns_result_to_ipv4_string(Interp* interp, void* result) {
    // struct addrinfo offsets (x86_64 glibc): flags=0, family=4, socktype=8, protocol=12, addrlen=16, addr=24
    int ai_family = *((int*)((char*)result + 4));
    void* ai_addr = *((void**)((char*)result + 24));

    char[64] ip_buf;  // buffer: C-interop (keep) — output buffer for c_inet_ntop
    if (ai_family != AF_INET) {
        // fault: lisp::DNS_FAILED
        return io_raise(interp, "io/dns-resolve-ipv6-not-supported", "dns-resolve: IPv6 not yet supported");
    }

    // sockaddr_in: sin_addr is at offset 4 (after sin_family + sin_port)
    void* sin_addr = (char*)ai_addr + 4;
    c_inet_ntop(AF_INET, sin_addr, &ip_buf, 64);

    usz ip_len = 0;
    while (ip_buf[ip_len] != 0 && ip_len < 63) ip_len++;
    return make_string(interp, ip_buf[:ip_len]);
}

fn Value* tcp_raise_fiber_required(Interp* interp, char[] op_name, char[] code) {
    char[96] ebuf;
    char[] msg = io::bprintf(&ebuf, "%s: requires running fiber context", (ZString)op_name)!!;
    return io_raise(interp, code, msg);
}

fn Value* tcp_raise_loop_unavailable(Interp* interp, char[] op_name, char[] code) {
    char[96] ebuf;
    char[] msg = io::bprintf(&ebuf, "%s: libuv loop unavailable", (ZString)op_name)!!;
    return io_raise(interp, code, msg);
}

// ============================================================
// (tcp-connect host port) — async connect in fiber context, returns handle
// ============================================================

fn Value* prim_tcp_connect(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 2) return io_raise(interp, "io/tcp-connect-expected-host-port", "tcp-connect: expected (tcp-connect host port)");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[0])) return io_raise(interp, "io/tcp-connect-host-not-string", "tcp-connect: host must be a string");
    // fault: lisp::EXPECTED_INT
    if (!is_int(args[1])) return io_raise(interp, "io/tcp-connect-port-not-integer", "tcp-connect: port must be an integer");

    int port = (int)args[1].int_val;

    if (!scheduler_is_running_fiber_context()) {
        return tcp_raise_fiber_required(interp, "tcp-connect", "io/tcp-connect-fiber-required");
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return tcp_raise_loop_unavailable(interp, "tcp-connect", "io/tcp-connect-libuv-unavailable");
    }

    usz fiber_id = g_scheduler.current_fiber_id;
    PendingTcpConnect* pending = &g_scheduler.pending_connects[fiber_id];
    if (pending.active) {
        return io_raise(
            interp,
            "io/tcp-connect-pending-request-exists",
            "tcp-connect: pending request already exists for this fiber"
        );
    }

    Value*[1] dns_args;
    dns_args[0] = args[0];
    Value* resolved = prim_dns_resolve(dns_args[..], null, interp);
    if (resolved == null || resolved.tag == ERROR) return resolved;
    if (!is_string(resolved)) {
        return io_raise(
            interp,
            "io/tcp-connect-resolution-invalid-result",
            "tcp-connect: DNS resolution returned invalid result"
        );
    }

    char* ip_copy = copy_to_heap_c_string(resolved.str_chars[:resolved.str_len]);
    if (ip_copy == null) {
        return io_raise(interp, "io/tcp-connect-out-of-memory", "tcp-connect: out of memory");
    }

    void* req_handle = null;
    void* tcp_handle = null;
    int start_status = omni_uv_tcp_connect_start(
        g_scheduler.uv_loop,
        ip_copy,
        port,
        &scheduler_uv_tcp_connect_cb,
        &req_handle,
        &tcp_handle
    );
    mem::free(ip_copy);
    if (start_status != 0 || req_handle == null || tcp_handle == null) {
        return io_raise(interp, "io/tcp-connect-async-init-failed", "tcp-connect: async connect init failed");
    }

    pending.active = true;
    pending.completed = false;
    pending.status = 0;
    pending.req_handle = req_handle;
    pending.tcp_handle = tcp_handle;

    g_scheduler.fibers[fiber_id].state = FIBER_BLOCKED;

    Value*[1] yargs;
    yargs[0] = make_nil(interp);
    Value* y = prim_yield(yargs[..], null, interp);
    if (y != null && y.tag == ERROR) return y;

    if (!pending.active || !pending.completed || pending.req_handle == null || pending.tcp_handle == null) {
        return io_raise(
            interp,
            "io/tcp-connect-resumed-before-completion",
            "tcp-connect: resumed before connect completion"
        );
    }

    int status = pending.status;
    void* done_req = pending.req_handle;
    void* done_tcp = pending.tcp_handle;
    g_scheduler.pending_connects[fiber_id] = {};

    if (status != 0) {
        omni_uv_tcp_connect_req_free(done_req);
        omni_uv_tcp_close_and_free(done_tcp);
        return io_raise(interp, "io/tcp-connect-connection-failed", "tcp-connect: connection failed");
    }

    int fd = -1;
    int fd_status = omni_uv_tcp_connect_detach_fd(done_tcp, &fd);
    omni_uv_tcp_connect_req_free(done_req);
    omni_uv_tcp_close_and_free(done_tcp);
    if (fd_status != 0 || fd < 0) {
        return io_raise(interp, "io/tcp-connect-fd-detach-failed", "tcp-connect: failed to detach socket");
    }

    return make_tcp_handle(interp, fd);
}

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
    parse_tcp_read_options(args, &max_bytes, &timeout_ms);

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

    return make_nil(interp);
}

