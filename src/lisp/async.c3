module lisp;

import std::core::mem;
import std::io;
import main;

// ============================================================
// POSIX/libuv extern declarations — no C wrapper needed
// ============================================================

// POSIX sockets (blocking fast-path for TCP)
extern fn int c_socket(int domain, int type, int protocol) @extern("socket");
extern fn int c_connect(int sockfd, void* addr, uint addrlen) @extern("connect");
extern fn int c_bind(int sockfd, void* addr, uint addrlen) @extern("bind");
extern fn int c_listen(int sockfd, int backlog) @extern("listen");
extern fn int c_accept(int sockfd, void* addr, void* addrlen) @extern("accept");
extern fn long c_send(int sockfd, void* buf, usz len, int flags) @extern("send");
extern fn long c_recv(int sockfd, void* buf, usz len, int flags) @extern("recv");
extern fn int c_close_fd(int fd) @extern("close");
extern fn int c_setsockopt(int sockfd, int level, int optname, void* optval, uint optlen) @extern("setsockopt");
extern fn int c_getaddrinfo(char* node, char* service, void* hints, void** res) @extern("getaddrinfo");
extern fn void c_freeaddrinfo(void* res) @extern("freeaddrinfo");
extern fn char* c_inet_ntop(int af, void* src, char* dst, uint size) @extern("inet_ntop");
extern fn uint c_inet_addr(char* cp) @extern("inet_addr");

// libuv — scheduler async I/O and wake signaling
extern fn int uv_loop_init(void* loop) @extern("uv_loop_init");
extern fn int uv_run(void* loop, int mode) @extern("uv_run");
extern fn int uv_loop_close(void* loop) @extern("uv_loop_close");
extern fn usz uv_loop_size() @extern("uv_loop_size");
extern fn usz uv_handle_size(int handle_type) @extern("uv_handle_size");
alias UvPollCb = fn void(void* handle, int status, int events);
alias UvCloseCb = fn void(void* handle);
alias UvTimerCb = fn void(void* handle);
alias UvAsyncCb = fn void(void* handle);
extern fn int uv_poll_init(void* loop, void* handle, int fd) @extern("uv_poll_init");
extern fn int uv_poll_start(void* handle, int events, UvPollCb cb) @extern("uv_poll_start");
extern fn int uv_poll_stop(void* handle) @extern("uv_poll_stop");
extern fn int uv_timer_init(void* loop, void* handle) @extern("uv_timer_init");
extern fn int uv_timer_start(void* handle, UvTimerCb cb, ulong timeout, ulong repeat) @extern("uv_timer_start");
extern fn int uv_timer_stop(void* handle) @extern("uv_timer_stop");
extern fn int uv_async_init(void* loop, void* handle, UvAsyncCb cb) @extern("uv_async_init");
extern fn int uv_async_send(void* handle) @extern("uv_async_send");
extern fn void uv_close(void* handle, UvCloseCb cb) @extern("uv_close");
extern fn int uv_loop_alive(void* loop) @extern("uv_loop_alive");
const int UV_HANDLE_ASYNC = 1;
const int UV_HANDLE_POLL = 8;
const int UV_HANDLE_TIMER = 13;
const int UV_READABLE = 1;
const int UV_RUN_DEFAULT = 0;
const int UV_RUN_ONCE = 1;
const int UV_RUN_NOWAIT = 2;

// errno helpers (Linux/glibc)
extern fn int* c_errno_location() @extern("__errno_location");
const int ERRNO_EAGAIN = 11;
const int ERRNO_EWOULDBLOCK = 11;
const int MSG_DONTWAIT = 0x40;

// sockaddr_in layout (for IPv4)
struct SockaddrIn {
    ushort sin_family;
    ushort sin_port;
    uint   sin_addr;
    char[8] sin_zero;  // buffer: C-interop (keep) — POSIX sockaddr_in struct padding
}

// addrinfo result — we only need ai_family, ai_addrlen, ai_addr, ai_next
// offsets vary by platform, so we read fields via helpers
const int AF_INET = 2;
const int SOCK_STREAM = 1;
const int IPPROTO_TCP = 6;
const int SOL_SOCKET = 1;
const int SO_REUSEADDR = 2;

// htons
fn ushort htons(ushort val) {
    return (ushort)(((val & 0xFF) << 8) | ((val >> 8) & 0xFF));
}

// ============================================================
// TCP Handle — wraps a file descriptor
// ============================================================

enum TcpHandleMode : char {
    TCP_STREAM,
    TCP_LISTENER
}

struct TcpHandle {
    int fd;
    bool connected;
    TcpHandleMode mode;
}

fn void tcp_handle_finalizer(void* handle) {
    TcpHandle* th = (TcpHandle*)handle;
    if (th == null) return;
    if (th.connected && th.fd >= 0) {
        c_close_fd(th.fd);
        th.connected = false;
        th.fd = -1;
    }
}

fn Value* make_tcp_handle(Interp* interp, int fd) {
    TcpHandle* th = (TcpHandle*)mem::malloc(TcpHandle.sizeof);
    if (th == null) return raise_error(interp, "tcp-connect: out of memory");
    th.fd = fd;
    th.connected = true;
    th.mode = TCP_STREAM;
    Value* v = make_ffi_handle_ex(interp, (void*)th, "tcp-handle",
        &tcp_handle_finalizer, true, false);
    if (v == null) {
        mem::free(th);
        return raise_error(interp, "tcp-connect: out of memory");
    }
    return v;
}

fn TcpHandle* get_tcp_handle(Value* v) {
    if (v == null || v.tag != FFI_HANDLE) return null;
    if (v.ffi_val == null) return null;
    return (TcpHandle*)v.ffi_val.lib_handle;
}

fn Value* make_tcp_listener_handle(Interp* interp, int fd) {
    TcpHandle* th = (TcpHandle*)mem::malloc(TcpHandle.sizeof);
    if (th == null) return raise_error(interp, "tcp-listen: out of memory");
    th.fd = fd;
    th.connected = true;
    th.mode = TCP_LISTENER;
    Value* v = make_ffi_handle_ex(interp, (void*)th, "tcp-handle",
        &tcp_handle_finalizer, true, false);
    if (v == null) {
        mem::free(th);
        return raise_error(interp, "tcp-listen: out of memory");
    }
    return v;
}

fn void copy_to_c_buffer(char* dst, usz cap, char[] src) {
    if (cap == 0) return;
    usz n = src.len < (cap - 1) ? src.len : (cap - 1);
    for (usz i = 0; i < n; i++) dst[i] = src[i];
    dst[n] = 0;
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

    char[64] host_buf;
    copy_to_c_buffer(&host_buf, host_buf.len, host);

    SockaddrIn addr = {};
    addr.sin_family = (ushort)AF_INET;
    addr.sin_port = htons((ushort)port);
    if (host.len == 9 && host[0] == 'l' && host[1] == 'o' && host[2] == 'c') {
        addr.sin_addr = c_inet_addr("127.0.0.1");
    } else {
        addr.sin_addr = c_inet_addr(&host_buf);
    }
    if (addr.sin_addr == 0xFFFFFFFFu) {
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
        return raise_error(interp, "dns-resolve: IPv6 not yet supported");
    }

    // sockaddr_in: sin_addr is at offset 4 (after sin_family + sin_port)
    void* sin_addr = (char*)ai_addr + 4;
    c_inet_ntop(AF_INET, sin_addr, &ip_buf, 64);

    usz ip_len = 0;
    while (ip_buf[ip_len] != 0 && ip_len < 63) ip_len++;
    return make_string(interp, ip_buf[:ip_len]);
}

// ============================================================
// (tcp-connect host port) — blocking connect, returns handle
// ============================================================

fn Value* prim_tcp_connect(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 2) return raise_error(interp, "tcp-connect: expected (tcp-connect host port)");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[0])) return raise_error(interp, "tcp-connect: host must be a string");
    // fault: lisp::EXPECTED_INT
    if (!is_int(args[1])) return raise_error(interp, "tcp-connect: port must be an integer");

    char[] host = args[0].str_chars[:args[0].str_len];
    int port = (int)args[1].int_val;

    // Resolve hostname via getaddrinfo
    void* result = null;
    if (!resolve_host_and_port(host, port, &result)) {
        // fault: lisp::DNS_FAILED
        return raise_error(interp, "tcp-connect: DNS resolution failed");
    }

    int fd = connect_fd_from_addrinfo(result);
    c_freeaddrinfo(result);
    if (fd < 0) {
        // fault: lisp::CONNECTION_REFUSED
        return raise_error(interp, "tcp-connect: connection failed");
    }

    return make_tcp_handle(interp, fd);
}

// ============================================================
// (tcp-listen host port [backlog]) — open listener socket
// ============================================================
fn Value* prim_tcp_listen(Value*[] args, Env* env, Interp* interp) {
    Value* host_v = null;
    Value* port_v = null;
    Value* backlog_v = null;
    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        host_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) return raise_error(interp, "tcp-listen: expected (tcp-listen host port [backlog])");
        port_v = car(packed);
        packed = cdr(packed);
        if (is_cons(packed)) backlog_v = car(packed);
    } else {
        if (args.len < 2) return raise_error(interp, "tcp-listen: expected (tcp-listen host port [backlog])");
        host_v = args[0];
        port_v = args[1];
        if (args.len >= 3) backlog_v = args[2];
    }

    if (!is_string(host_v)) return raise_error(interp, "tcp-listen: host must be a string");
    if (!is_int(port_v)) return raise_error(interp, "tcp-listen: port must be an integer");

    int backlog = 128;
    if (backlog_v != null) {
        if (!is_int(backlog_v)) return raise_error(interp, "tcp-listen: backlog must be an integer");
        backlog = (int)backlog_v.int_val;
        if (backlog < 1) backlog = 1;
        if (backlog > 1024) backlog = 1024;
    }

    char[] host = host_v.str_chars[:host_v.str_len];
    int port = (int)port_v.int_val;
    int fd = listen_fd_from_host(host, port, backlog);
    if (fd < 0) return raise_error(interp, "tcp-listen: bind/listen failed");
    return make_tcp_listener_handle(interp, fd);
}

fn Value* tcp_accept_with_offload(TcpHandle* listener, Interp* interp) {
    Value* op = make_symbol(interp, interp.symbols.intern("accept-fd"));
    Value* fd = make_int(interp, listener.fd);
    Value* job = make_cons(interp, op, make_cons(interp, fd, make_nil(interp)));
    Value*[1] off_args;
    off_args[0] = job;
    Value* accepted = prim_offload(off_args[..], null, interp);
    if (accepted == null || accepted.tag == ERROR) return accepted;
    if (!is_int(accepted) || accepted.int_val < 0) {
        return raise_error(interp, "tcp-accept: offload returned invalid fd");
    }
    return make_tcp_handle(interp, (int)accepted.int_val);
}

// ============================================================
// (tcp-accept listener-handle) — accept one connection
// ============================================================
fn Value* prim_tcp_accept(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "tcp-accept: expected (tcp-accept listener-handle)");
    TcpHandle* listener = get_tcp_handle(args[0]);
    if (listener == null || !listener.connected || listener.mode != TCP_LISTENER) {
        return raise_error(interp, "tcp-accept: invalid or closed listener handle");
    }

    if (scheduler_is_running_fiber_context()) {
        return tcp_accept_with_offload(listener, interp);
    }

    int fd = tcp_accept_fd(listener.fd);
    if (fd < 0) return raise_error(interp, "tcp-accept: accept failed");
    return make_tcp_handle(interp, fd);
}

// ============================================================
// (tcp-write handle data) — blocking write, returns bytes written
// ============================================================

fn Value* prim_tcp_write(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 2) return raise_error(interp, "tcp-write: expected (tcp-write handle data)");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[1])) return raise_error(interp, "tcp-write: data must be a string");

    TcpHandle* th = get_tcp_handle(args[0]);
    // fault: lisp::TYPE_MISMATCH
    if (th == null || !th.connected || th.mode != TCP_STREAM) return raise_error(interp, "tcp-write: invalid or closed stream handle");

    char[] data = args[1].str_chars[:args[1].str_len];
    // fault: lisp::WRITE_FAILED
    long sent = c_send(th.fd, data.ptr, data.len, 0);
    if (sent < 0) return raise_error(interp, "tcp-write: send failed");

    return make_int(interp, sent);
}

// ============================================================
// (tcp-read handle [max-bytes]) — blocking read, returns string
// ============================================================

fn Value* prim_tcp_read(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return raise_error(interp, "tcp-read: expected (tcp-read handle [max-bytes])");

    TcpHandle* th = get_tcp_handle(args[0]);
    // fault: lisp::TYPE_MISMATCH
    if (th == null || !th.connected || th.mode != TCP_STREAM) return raise_error(interp, "tcp-read: invalid or closed stream handle");

    usz max_bytes = 0;
    long timeout_ms = -1;
    parse_tcp_read_options(args, &max_bytes, &timeout_ms);

    // Async bridge: when called from a scheduler-managed fiber, register a non-blocking
    // read with libuv and suspend this fiber until data is ready.
    Value* async_result = scheduler_try_async_tcp_read(th.fd, max_bytes, timeout_ms, interp);
    if (async_result != null) return async_result;

    char* buf = (char*)mem::malloc(max_bytes);
    // fault: lisp::READ_FAILED
    if (buf == null) return raise_error(interp, "tcp-read: out of memory");
    defer mem::free(buf);

    long received = c_recv(th.fd, buf, max_bytes, 0);
    if (received < 0) {
        // fault: lisp::READ_FAILED
        return raise_error(interp, "tcp-read: recv failed");
    }
    if (received == 0) {
        return make_string(interp, "");  // EOF
    }

    Value* result = make_string(interp, buf[:(usz)received]);
    return result;
}

// ============================================================
// (tcp-read-timeout handle timeout-ms) — read with timeout
// ============================================================

fn Value* prim_tcp_read_timeout(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "tcp-read-timeout: expected (tcp-read-timeout handle timeout-ms)");
    if (!is_int(args[1])) return raise_error(interp, "tcp-read-timeout: timeout-ms must be an integer");

    Value*[3] read_args;
    read_args[0] = args[0];             // handle
    read_args[1] = make_int(interp, 4096); // default max-bytes
    read_args[2] = args[1];             // timeout-ms
    return prim_tcp_read(read_args[..], env, interp);
}

// ============================================================
// (tcp-close handle) — close connection
// ============================================================

fn Value* prim_tcp_close(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return raise_error(interp, "tcp-close: expected (tcp-close handle)");

    TcpHandle* th = get_tcp_handle(args[0]);
    // fault: lisp::TYPE_MISMATCH
    if (th == null) return raise_error(interp, "tcp-close: invalid handle");

    if (th.connected) {
        c_close_fd(th.fd);
        th.connected = false;
        th.fd = -1;
    }

    return make_nil(interp);
}

// ============================================================
// (dns-resolve hostname) — resolve to IP string
// ============================================================

fn Value* prim_dns_resolve(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return raise_error(interp, "dns-resolve: expected (dns-resolve hostname)");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[0])) return raise_error(interp, "dns-resolve: hostname must be a string");

    char[] host = args[0].str_chars[:args[0].str_len];
    char[256] host_buf;  // buffer: C-interop (keep) — null-terminated for c_getaddrinfo
    copy_to_c_buffer(&host_buf, host_buf.len, host);

    void* result = null;
    int status = c_getaddrinfo(&host_buf, null, null, &result);
    if (status != 0 || result == null) {
        // fault: lisp::DNS_FAILED
        return raise_error(interp, "dns-resolve: resolution failed");
    }

    Value* resolved = dns_result_to_ipv4_string(interp, result);
    c_freeaddrinfo(result);
    return resolved;
}

// ============================================================
// (async-sleep ms) — blocking sleep (milliseconds)
// ============================================================

fn Value* prim_async_sleep(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return raise_error(interp, "async-sleep: expected (async-sleep ms)");
    // fault: lisp::EXPECTED_INT
    if (!is_int(args[0])) return raise_error(interp, "async-sleep: ms must be an integer");

    long ms = args[0].int_val;
    if (ms > 0) {
        c_usleep((uint)(ms * 1000));
    }

    return make_nil(interp);
}
