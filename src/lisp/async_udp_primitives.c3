module lisp;

import std::core::mem;

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
    return make_void(interp);
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
    return make_void(interp);
}
