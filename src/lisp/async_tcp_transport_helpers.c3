module lisp;

import std::io;

extern fn int omni_addrinfo_connect_fd(void* result) @extern("omni_addrinfo_connect_fd");
extern fn int omni_addrinfo_render_ip(void* result, char* dst, uint dst_size) @extern("omni_addrinfo_render_ip");

const int OMNI_ADDRINFO_RENDER_OK = 0;
const int OMNI_ADDRINFO_RENDER_UNSUPPORTED_FAMILY = 1;
const int OMNI_ADDRINFO_RENDER_FAILED = -1;

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
    if (try char[] rendered_port = io::bprintf(&port_buf, "%d", port)) {
        int plen = rendered_port.len;
        if ((usz)plen >= port_buf.len) {
            port_buf[port_buf.len - 1] = 0;
        } else {
            port_buf[(usz)plen] = 0;
        }
    } else {
        return false;
    }
    return c_getaddrinfo(&host_buf, &port_buf, null, out_result) == 0 && *out_result != null;
}

fn int connect_fd_from_addrinfo(void* result) {
    return omni_addrinfo_connect_fd(result);
}

fn int listen_fd_from_host(char[] host, int port, int backlog) {
    int fd = c_socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (fd < 0) return -1;

    int reuse = 1;
    if (c_setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &reuse, 4) < 0) {
        c_close_fd(fd);
        return -1;
    }

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

fn Value* parse_tcp_read_option_values(Value* max_bytes_v, Value* timeout_ms_v, usz* max_bytes, long* timeout_ms, Interp* interp) {
    *max_bytes = ASYNC_IO_TCP_READ_DEFAULT_MAX_BYTES;
    if (max_bytes_v != null) {
        if (!is_int(max_bytes_v)) {
            return io_raise(interp, "io/tcp-read-max-bytes-not-integer", "tcp-read: max-bytes must be an integer");
        }
        if (max_bytes_v.int_val <= 0) {
            return io_raise(interp, "io/tcp-read-max-bytes-invalid", "tcp-read: max-bytes must be > 0");
        }
        *max_bytes = (usz)max_bytes_v.int_val;
        if (*max_bytes > ASYNC_IO_TCP_READ_HARD_MAX_BYTES) {
            return io_raise(interp, "io/tcp-read-max-bytes-too-large", "tcp-read: max-bytes must be <= 65536");
        }
    }
    *timeout_ms = -1;
    if (timeout_ms_v != null) {
        if (!is_int(timeout_ms_v)) {
            return io_raise(interp, "io/tcp-read-timeout-not-integer", "tcp-read: timeout-ms must be an integer");
        }
        *timeout_ms = timeout_ms_v.int_val;
        if (*timeout_ms < 0) *timeout_ms = -1;
    }
    return null;
}

fn Value* parse_tcp_read_args(Value*[] args, Value** handle_v, usz* max_bytes, long* timeout_ms, Interp* interp) {
    Value* max_bytes_v = null;
    Value* timeout_ms_v = null;

    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        *handle_v = car(packed);
        packed = cdr(packed);
        if (is_cons(packed)) {
            max_bytes_v = car(packed);
            packed = cdr(packed);
        }
        if (is_cons(packed)) {
            timeout_ms_v = car(packed);
            packed = cdr(packed);
        }
        if (!is_nil(packed)) {
            return io_raise(interp, "io/tcp-read-expected-handle-max-bytes", "tcp-read: expected (tcp-read handle [max-bytes] [timeout-ms])");
        }
    } else {
        if (args.len < 1 || args.len > 3) {
            return io_raise(interp, "io/tcp-read-expected-handle-max-bytes", "tcp-read: expected (tcp-read handle [max-bytes] [timeout-ms])");
        }
        *handle_v = args[0];
        if (args.len >= 2) max_bytes_v = args[1];
        if (args.len >= 3) timeout_ms_v = args[2];
    }

    return parse_tcp_read_option_values(max_bytes_v, timeout_ms_v, max_bytes, timeout_ms, interp);
}

fn Value* dns_result_to_ip_string(Interp* interp, void* result) {
    char[64] ip_buf;  // buffer: C-interop (keep) — output buffer for c_inet_ntop
    int render_status = omni_addrinfo_render_ip(result, &ip_buf, 64);
    if (render_status == OMNI_ADDRINFO_RENDER_UNSUPPORTED_FAMILY) {
        return io_raise(interp, "io/dns-resolve-unsupported-family", "dns-resolve: unsupported address family");
    }
    if (render_status != OMNI_ADDRINFO_RENDER_OK) {
        return io_raise(interp, "io/dns-resolve-render-failed", "dns-resolve: failed to format resolved address");
    }

    usz ip_len = 0;
    while (ip_buf[ip_len] != 0 && ip_len < 63) ip_len++;
    return make_string(interp, ip_buf[:ip_len]);
}

fn Value* tcp_raise_fiber_required(Interp* interp, char[] op_name, char[] code) {
    return async_io_raise_fiber_required(interp, op_name, code, "tcp op: requires running fiber context");
}

fn Value* tcp_raise_loop_unavailable(Interp* interp, char[] op_name, char[] code) {
    return async_io_raise_loop_unavailable(interp, op_name, code, "tcp op: libuv loop unavailable");
}
