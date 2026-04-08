module lisp;

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
    char* rendered = c_inet_ntop(AF_INET, sin_addr, &ip_buf, 64);
    if (rendered == null) {
        return io_raise(interp, "io/dns-resolve-render-failed", "dns-resolve: failed to format IPv4 address");
    }

    usz ip_len = 0;
    while (ip_buf[ip_len] != 0 && ip_len < 63) ip_len++;
    return make_string(interp, ip_buf[:ip_len]);
}

fn Value* tcp_raise_fiber_required(Interp* interp, char[] op_name, char[] code) {
    char[96] ebuf;
    if (try char[] msg = io::bprintf(&ebuf, "%s: requires running fiber context", (ZString)op_name)) {
        return io_raise(interp, code, msg);
    }
    return io_raise(interp, code, "tcp op: requires running fiber context");
}

fn Value* tcp_raise_loop_unavailable(Interp* interp, char[] op_name, char[] code) {
    char[96] ebuf;
    if (try char[] msg = io::bprintf(&ebuf, "%s: libuv loop unavailable", (ZString)op_name)) {
        return io_raise(interp, code, msg);
    }
    return io_raise(interp, code, "tcp op: libuv loop unavailable");
}
