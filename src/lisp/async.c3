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
extern fn long c_sendto(int sockfd, void* buf, usz len, int flags, void* dest_addr, uint addrlen) @extern("sendto");
extern fn long c_recv(int sockfd, void* buf, usz len, int flags) @extern("recv");
extern fn int c_close_fd(int fd) @extern("close");
extern fn int c_unlink(char* path) @extern("unlink");
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
alias UvGetAddrInfoCb = fn void(void* req, int status, void* res);
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
extern fn void* omni_uv_getaddrinfo_req_new() @extern("omni_uv_getaddrinfo_req_new");
extern fn void omni_uv_getaddrinfo_req_free(void* req) @extern("omni_uv_getaddrinfo_req_free");
extern fn int omni_uv_getaddrinfo_start(void* loop, void* req, UvGetAddrInfoCb cb, char* node, char* service) @extern("omni_uv_getaddrinfo_start");
extern fn int omni_uv_cancel_req(void* req) @extern("omni_uv_cancel_req");
extern fn void omni_uv_req_set_data(void* req, void* data) @extern("omni_uv_req_set_data");
extern fn void* omni_uv_req_get_data(void* req) @extern("omni_uv_req_get_data");
extern fn void omni_uv_freeaddrinfo(void* res) @extern("omni_uv_freeaddrinfo");
extern fn int omni_uv_process_spawn(
    char* file,
    void* argv,
    void* envp,
    void** out_process,
    int* stdin_fd_out,
    int* stdout_fd_out,
    int* stderr_fd_out
) @extern("omni_uv_process_spawn");
extern fn int omni_uv_process_wait(void* process, int* exit_status_out, int* term_signal_out) @extern("omni_uv_process_wait");
extern fn int omni_uv_process_kill(void* process, int sig) @extern("omni_uv_process_kill");
extern fn int omni_uv_process_pid(void* process) @extern("omni_uv_process_pid");
extern fn void omni_uv_process_close(void* process) @extern("omni_uv_process_close");
extern fn int omni_uv_signal_start(int signum, void** out_watch) @extern("omni_uv_signal_start");
extern fn int omni_uv_signal_poll_pending(void* signal_watch) @extern("omni_uv_signal_poll_pending");
extern fn int omni_uv_signal_stop(void* signal_watch) @extern("omni_uv_signal_stop");
extern fn int omni_uv_pipe_connect_fd(char* path, int* out_fd) @extern("omni_uv_pipe_connect_fd");
extern fn int omni_uv_pipe_listen_fd(char* path, int backlog, int* out_fd) @extern("omni_uv_pipe_listen_fd");
const int UV_HANDLE_ASYNC = 1;
const int UV_HANDLE_POLL = 8;
const int UV_HANDLE_TIMER = 13;
const int UV_READABLE = 1;
const int UV_WRITABLE = 2;
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

// sockaddr_un layout (for Unix domain sockets)
struct SockaddrUn {
    ushort sun_family;
    char[108] sun_path;  // buffer: C-interop (keep) — POSIX sockaddr_un path payload
}

// addrinfo result — we only need ai_family, ai_addrlen, ai_addr, ai_next
// offsets vary by platform, so we read fields via helpers
const int AF_UNIX = 1;
const int AF_INET = 2;
const int SOCK_STREAM = 1;
const int SOCK_DGRAM = 2;
const int IPPROTO_TCP = 6;
const int IPPROTO_UDP = 17;
const int SOL_SOCKET = 1;
const int SO_REUSEADDR = 2;
const int SIGKILL_VAL = 9;

// htons
fn ushort htons(ushort val) {
    return (ushort)(((val & 0xFF) << 8) | ((val >> 8) & 0xFF));
}

fn Value* io_raise(Interp* interp, char[] code, char[] message, Value* data = null) @inline {
    return raise_error_with_payload_names(interp, "io", code, message, data);
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

struct UdpHandle {
    int fd;
    bool open;
}

struct ProcessHandle {
    void* uv_process;
    bool open;
}

struct SignalHandle {
    void* uv_signal_watch;
    Value* callback;
    int signum;
    bool open;
}

const usz MAX_SIGNAL_HANDLES = 32;
SignalHandle*[MAX_SIGNAL_HANDLES] g_signal_handles;

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
    if (th == null) return io_raise(interp, "io/tcp-connect-out-of-memory", "tcp-connect: out of memory");
    th.fd = fd;
    th.connected = true;
    th.mode = TCP_STREAM;
    Value* v = make_ffi_handle_ex(interp, (void*)th, "tcp-handle",
        &tcp_handle_finalizer, true, false);
    if (v == null) {
        mem::free(th);
        return io_raise(interp, "io/tcp-connect-out-of-memory", "tcp-connect: out of memory");
    }
    return v;
}

fn TcpHandle* get_tcp_handle(Value* v) {
    if (v == null || v.tag != FFI_HANDLE) return null;
    if (v.ffi_val == null) return null;
    return (TcpHandle*)v.ffi_val.lib_handle;
}

fn void udp_handle_finalizer(void* handle) {
    UdpHandle* uh = (UdpHandle*)handle;
    if (uh == null) return;
    if (uh.open && uh.fd >= 0) {
        c_close_fd(uh.fd);
        uh.open = false;
        uh.fd = -1;
    }
}

fn Value* make_udp_handle(Interp* interp, int fd) {
    UdpHandle* uh = (UdpHandle*)mem::malloc(UdpHandle.sizeof);
    if (uh == null) {
        c_close_fd(fd);
        return io_raise(interp, "io/udp-socket-out-of-memory", "udp-socket: out of memory");
    }
    uh.fd = fd;
    uh.open = true;
    Value* v = make_ffi_handle_ex(interp, (void*)uh, "udp-handle",
        &udp_handle_finalizer, true, false);
    if (v == null) {
        mem::free(uh);
        c_close_fd(fd);
        return io_raise(interp, "io/udp-socket-out-of-memory", "udp-socket: out of memory");
    }
    return v;
}

fn UdpHandle* get_udp_handle(Value* v) {
    if (v == null || v.tag != FFI_HANDLE) return null;
    if (v.ffi_val == null) return null;
    return (UdpHandle*)v.ffi_val.lib_handle;
}

fn void process_handle_finalizer(void* handle) {
    ProcessHandle* ph = (ProcessHandle*)handle;
    if (ph == null) return;
    if (ph.open && ph.uv_process != null) {
        omni_uv_process_close(ph.uv_process);
        ph.uv_process = null;
        ph.open = false;
    }
}

fn bool signal_callback_callable(Value* v) @inline {
    if (v == null) return false;
    ValueTag t = v.tag;
    return t == CLOSURE || t == PRIMITIVE || t == PARTIAL_PRIM || t == METHOD_TABLE;
}

fn bool register_signal_handle(SignalHandle* sh) {
    if (sh == null) return false;
    for (usz i = 0; i < MAX_SIGNAL_HANDLES; i++) {
        if (g_signal_handles[i] != null) continue;
        g_signal_handles[i] = sh;
        return true;
    }
    return false;
}

fn void unregister_signal_handle(SignalHandle* sh) {
    if (sh == null) return;
    for (usz i = 0; i < MAX_SIGNAL_HANDLES; i++) {
        if (g_signal_handles[i] != sh) continue;
        g_signal_handles[i] = null;
        return;
    }
}

fn void close_signal_handle(SignalHandle* sh) {
    if (sh == null || !sh.open || sh.uv_signal_watch == null) return;
    (void)omni_uv_signal_stop(sh.uv_signal_watch);
    sh.uv_signal_watch = null;
    sh.open = false;
}

fn void signal_handle_finalizer(void* handle) {
    SignalHandle* sh = (SignalHandle*)handle;
    if (sh == null) return;
    unregister_signal_handle(sh);
    close_signal_handle(sh);
    sh.callback = null;
}

fn bool ffi_handle_name_equals(Value* v, char[] expected) @inline {
    if (v == null || v.tag != FFI_HANDLE || v.ffi_val == null) return false;
    if (v.ffi_val.name_len != expected.len) return false;
    for (usz i = 0; i < expected.len; i++) {
        if (v.ffi_val.lib_name[i] != expected[i]) return false;
    }
    return true;
}

fn ProcessHandle* get_process_handle(Value* v) {
    if (!ffi_handle_name_equals(v, "process-handle")) return null;
    return (ProcessHandle*)v.ffi_val.lib_handle;
}

fn Value* make_process_handle(Interp* interp, void* uv_process) {
    ProcessHandle* ph = (ProcessHandle*)mem::malloc(ProcessHandle.sizeof);
    if (ph == null) {
        if (uv_process != null) omni_uv_process_close(uv_process);
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    ph.uv_process = uv_process;
    ph.open = true;
    Value* v = make_ffi_handle_ex(interp, (void*)ph, "process-handle",
        &process_handle_finalizer, true, false);
    if (v == null) {
        mem::free(ph);
        if (uv_process != null) omni_uv_process_close(uv_process);
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    return v;
}

fn SignalHandle* get_signal_handle(Value* v) {
    if (!ffi_handle_name_equals(v, "signal-handle")) return null;
    return (SignalHandle*)v.ffi_val.lib_handle;
}

fn Value* make_signal_handle(Interp* interp, void* uv_signal_watch, int signum, Value* callback_v) {
    if (uv_signal_watch == null) {
        return io_raise(interp, "io/signal-handle-failed", "signal-handle: failed to create signal watcher");
    }

    SignalHandle* sh = (SignalHandle*)mem::malloc(SignalHandle.sizeof);
    if (sh == null) {
        (void)omni_uv_signal_stop(uv_signal_watch);
        return io_raise(interp, "io/signal-handle-out-of-memory", "signal-handle: out of memory");
    }

    Value* callback_root = boundary_promote_to_root(callback_v, interp);
    if (callback_root == null) {
        (void)omni_uv_signal_stop(uv_signal_watch);
        mem::free(sh);
        return io_raise(interp, "io/signal-handle-out-of-memory", "signal-handle: out of memory");
    }

    sh.uv_signal_watch = uv_signal_watch;
    sh.callback = callback_root;
    sh.signum = signum;
    sh.open = true;

    if (!register_signal_handle(sh)) {
        close_signal_handle(sh);
        mem::free(sh);
        return io_raise(interp, "io/signal-handle-limit-reached", "signal-handle: max active signal handlers reached");
    }

    Value* v = make_ffi_handle_ex(interp, (void*)sh, "signal-handle",
        &signal_handle_finalizer, true, false);
    if (v == null) {
        unregister_signal_handle(sh);
        close_signal_handle(sh);
        mem::free(sh);
        return io_raise(interp, "io/signal-handle-out-of-memory", "signal-handle: out of memory");
    }
    return v;
}

fn void dispatch_signal_callback(SignalHandle* sh, int signum, Interp* interp) {
    if (sh == null || !sh.open || sh.callback == null) return;
    if (!signal_callback_callable(sh.callback)) return;

    Value* arg = make_int(interp, (long)signum);
    Value* result = jit_apply_value(sh.callback, arg, interp);
    if (result != null && result.tag == ERROR) {
        // Keep signal delivery non-fatal: callback failures should not crash runtime loops.
        return;
    }
}

fn void drain_signal_handle_pending(SignalHandle* sh, Interp* interp) {
    if (sh == null || !sh.open || sh.uv_signal_watch == null) return;
    int pending = omni_uv_signal_poll_pending(sh.uv_signal_watch);
    if (pending <= 0) return;

    int capped = pending > 1024 ? 1024 : pending;
    for (int i = 0; i < capped; i++) {
        dispatch_signal_callback(sh, sh.signum, interp);
    }
}

fn void dispatch_pending_signal_callbacks(Interp* interp) {
    if (interp == null) return;
    for (usz i = 0; i < MAX_SIGNAL_HANDLES; i++) {
        SignalHandle* sh = g_signal_handles[i];
        if (sh == null) continue;
        drain_signal_handle_pending(sh, interp);
    }
}

fn void close_fs_like_handle(Value* v, Interp* interp) {
    if (!ffi_handle_name_equals(v, "fs-handle")) return;
    Value*[1] close_args;
    close_args[0] = v;
    (void)prim_fs_close(close_args[..], null, interp);
}

fn Value* make_tcp_listener_handle(Interp* interp, int fd) {
    TcpHandle* th = (TcpHandle*)mem::malloc(TcpHandle.sizeof);
    if (th == null) return io_raise(interp, "io/tcp-listen-out-of-memory", "tcp-listen: out of memory");
    th.fd = fd;
    th.connected = true;
    th.mode = TCP_LISTENER;
    Value* v = make_ffi_handle_ex(interp, (void*)th, "tcp-handle",
        &tcp_handle_finalizer, true, false);
    if (v == null) {
        mem::free(th);
        return io_raise(interp, "io/tcp-listen-out-of-memory", "tcp-listen: out of memory");
    }
    return v;
}

fn void copy_to_c_buffer(char* dst, usz cap, char[] src) {
    if (cap == 0) return;
    usz n = src.len < (cap - 1) ? src.len : (cap - 1);
    for (usz i = 0; i < n; i++) dst[i] = src[i];
    dst[n] = 0;
}

fn char* copy_to_heap_c_string(char[] src) {
    usz cap = src.len + 1;
    char* out = (char*)mem::malloc(cap);
    if (out == null) return null;
    for (usz i = 0; i < src.len; i++) out[i] = src[i];
    out[src.len] = 0;
    return out;
}

fn char* copy_value_to_heap_c_string(Value* v, Interp* interp) {
    if (v == null) return null;
    if (is_string(v)) return copy_to_heap_c_string(v.str_chars[:v.str_len]);
    if (is_symbol(v)) return copy_to_heap_c_string(interp.symbols.get_name(v.sym_val));
    return null;
}

fn void free_heap_c_string_items(char** items, usz len) {
    if (items == null) return;
    for (usz i = 0; i < len; i++) {
        if (items[i] != null) mem::free(items[i]);
    }
    mem::free(items);
}

fn Value* value_to_process_handle_arg(Value* v, Interp* interp) {
    if (v == null) return null;
    if (ffi_handle_name_equals(v, "process-handle")) return v;
    if (v.tag == HASHMAP && v.hashmap_val != null) {
        Value* handle_key = make_symbol(interp, interp.symbols.intern("handle"));
        return hashmap_get(v.hashmap_val, handle_key);
    }
    return null;
}

fn Value* collect_string_sequence(
    Value* seq,
    char*** out_items,
    usz* out_len,
    Interp* interp,
    char[] code,
    char[] message
) {
    *out_items = null;
    *out_len = 0;
    if (seq == null || is_nil(seq)) return null;

    usz count = 0;
    if (seq.tag == ARRAY && seq.array_val != null) {
        count = seq.array_val.length;
    } else if (is_cons(seq)) {
        Value* cur = seq;
        while (is_cons(cur)) {
            count++;
            cur = cdr(cur);
        }
        if (!is_nil(cur)) return io_raise(interp, code, message);
    } else {
        return io_raise(interp, code, message);
    }

    if (count == 0) return null;

    char** items = (char**)mem::malloc(char*.sizeof * count);
    if (items == null) {
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    for (usz i = 0; i < count; i++) items[i] = null;

    if (seq.tag == ARRAY && seq.array_val != null) {
        for (usz i = 0; i < count; i++) {
            Value* it = seq.array_val.items[i];
            if (!(is_string(it) || is_symbol(it))) {
                free_heap_c_string_items(items, count);
                return io_raise(interp, code, message);
            }
            char* copied = copy_value_to_heap_c_string(it, interp);
            if (copied == null) {
                free_heap_c_string_items(items, count);
                return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
            }
            items[i] = copied;
        }
    } else {
        Value* cur = seq;
        usz i = 0;
        while (is_cons(cur) && i < count) {
            Value* it = car(cur);
            if (!(is_string(it) || is_symbol(it))) {
                free_heap_c_string_items(items, count);
                return io_raise(interp, code, message);
            }
            char* copied = copy_value_to_heap_c_string(it, interp);
            if (copied == null) {
                free_heap_c_string_items(items, count);
                return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
            }
            items[i] = copied;
            i++;
            cur = cdr(cur);
        }
    }

    *out_items = items;
    *out_len = count;
    return null;
}

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

fn bool fill_sockaddr_unix(char[] path, SockaddrUn* out_addr) {
    if (out_addr == null) return false;
    if (path.len == 0) return false;
    if (path.len >= out_addr.sun_path.len) return false;

    out_addr.sun_family = (ushort)AF_UNIX;
    for (usz i = 0; i < path.len; i++) out_addr.sun_path[i] = path[i];
    out_addr.sun_path[path.len] = 0;
    return true;
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

fn int pipe_connect_fd(char[] path) {
    int fd = c_socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) return -1;

    SockaddrUn addr = {};
    if (!fill_sockaddr_unix(path, &addr)) {
        c_close_fd(fd);
        return -1;
    }

    if (c_connect(fd, &addr, SockaddrUn.sizeof) < 0) {
        c_close_fd(fd);
        return -1;
    }
    return fd;
}

fn int pipe_listen_fd(char[] path, int backlog) {
    int fd = c_socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) return -1;

    SockaddrUn addr = {};
    if (!fill_sockaddr_unix(path, &addr)) {
        c_close_fd(fd);
        return -1;
    }

    // Best-effort cleanup for stale socket path from previous runs.
    (void)c_unlink(&addr.sun_path[0]);

    if (c_bind(fd, &addr, SockaddrUn.sizeof) < 0) {
        c_close_fd(fd);
        return -1;
    }
    if (c_listen(fd, backlog) < 0) {
        c_close_fd(fd);
        return -1;
    }
    return fd;
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

// ============================================================
// (tcp-connect host port) — blocking connect, returns handle
// ============================================================

fn Value* prim_tcp_connect(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 2) return io_raise(interp, "io/tcp-connect-expected-host-port", "tcp-connect: expected (tcp-connect host port)");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[0])) return io_raise(interp, "io/tcp-connect-host-not-string", "tcp-connect: host must be a string");
    // fault: lisp::EXPECTED_INT
    if (!is_int(args[1])) return io_raise(interp, "io/tcp-connect-port-not-integer", "tcp-connect: port must be an integer");

    // In a fiber context, offload blocking DNS+connect to a worker thread.
    if (scheduler_is_running_fiber_context()) {
        Value* op = make_symbol(interp, interp.symbols.intern("tcp-connect"));
        Value* job = make_cons(interp, op,
                        make_cons(interp, args[0],
                          make_cons(interp, args[1], make_nil(interp))));
        Value* connected = scheduler_run_internal_io_offload_job(job, interp);
        if (connected == null || connected.tag == ERROR) return connected;
        if (!is_int(connected) || connected.int_val < 0) {
            return io_raise(interp, "io/tcp-connect-offload-invalid-fd", "tcp-connect: offload returned invalid fd");
        }
        return make_tcp_handle(interp, (int)connected.int_val);
    }

    char[] host = args[0].str_chars[:args[0].str_len];
    int port = (int)args[1].int_val;

    // Resolve hostname via getaddrinfo
    void* result = null;
    if (!resolve_host_and_port(host, port, &result)) {
        // fault: lisp::DNS_FAILED
        return io_raise(interp, "io/tcp-connect-dns-resolution-failed", "tcp-connect: DNS resolution failed");
    }

    int fd = connect_fd_from_addrinfo(result);
    c_freeaddrinfo(result);
    if (fd < 0) {
        // fault: lisp::CONNECTION_REFUSED
        return io_raise(interp, "io/tcp-connect-connection-failed", "tcp-connect: connection failed");
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

    char[] host = host_v.str_chars[:host_v.str_len];
    int port = (int)port_v.int_val;
    int fd = listen_fd_from_host(host, port, backlog);
    if (fd < 0) return io_raise(interp, "io/tcp-listen-bind-listen-failed", "tcp-listen: bind/listen failed");
    return make_tcp_listener_handle(interp, fd);
}

fn Value* tcp_accept_with_offload(TcpHandle* listener, Interp* interp) {
    Value* op = make_symbol(interp, interp.symbols.intern("accept-fd"));
    Value* fd = make_int(interp, listener.fd);
    Value* job = make_cons(interp, op, make_cons(interp, fd, make_nil(interp)));
    Value* accepted = scheduler_run_internal_io_offload_job(job, interp);
    if (accepted == null || accepted.tag == ERROR) return accepted;
    if (!is_int(accepted) || accepted.int_val < 0) {
        return io_raise(interp, "io/tcp-accept-offload-invalid-fd", "tcp-accept: offload returned invalid fd");
    }
    return make_tcp_handle(interp, (int)accepted.int_val);
}

// ============================================================
// (tcp-accept listener-handle) — accept one connection
// ============================================================
fn Value* prim_tcp_accept(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return io_raise(interp, "io/tcp-accept-expected-listener-handle", "tcp-accept: expected (tcp-accept listener-handle)");
    TcpHandle* listener = get_tcp_handle(args[0]);
    bool in_fiber = scheduler_is_running_fiber_context();
    if (listener == null || !listener.connected || listener.mode != TCP_LISTENER) {
        return io_raise(interp, "io/tcp-accept-invalid-listener-handle", "tcp-accept: invalid or closed listener handle");
    }

    if (in_fiber) {
        return tcp_accept_with_offload(listener, interp);
    }
    int fd = tcp_accept_fd(listener.fd);
    if (fd < 0) return io_raise(interp, "io/tcp-accept-failed", "tcp-accept: accept failed");
    return make_tcp_handle(interp, fd);
}

// ============================================================
// (tcp-write handle data) — blocking write, returns bytes written
// ============================================================

fn Value* prim_tcp_write(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 2) return io_raise(interp, "io/tcp-write-expected-handle-data", "tcp-write: expected (tcp-write handle data)");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[1])) return io_raise(interp, "io/tcp-write-data-not-string", "tcp-write: data must be a string");

    TcpHandle* th = get_tcp_handle(args[0]);
    // fault: lisp::TYPE_MISMATCH
    if (th == null || !th.connected || th.mode != TCP_STREAM) {
        return io_raise(interp, "io/tcp-write-invalid-stream-handle", "tcp-write: invalid or closed stream handle");
    }

    char[] data = args[1].str_chars[:args[1].str_len];
    // Async bridge: in scheduler-managed fiber context, prefer libuv writable
    // wakeups so writes do not block the runtime thread.
    Value* async_result = scheduler_try_async_tcp_write(th.fd, data, interp);
    if (async_result != null) return async_result;

    // fault: lisp::WRITE_FAILED
    long sent = c_send(th.fd, data.ptr, data.len, 0);
    if (sent < 0) return io_raise(interp, "io/tcp-write-send-failed", "tcp-write: send failed");

    return make_int(interp, sent);
}

// ============================================================
// (tcp-read handle [max-bytes]) — blocking read, returns string
// ============================================================

fn Value* prim_tcp_read(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return io_raise(interp, "io/tcp-read-expected-handle-max-bytes", "tcp-read: expected (tcp-read handle [max-bytes])");

    TcpHandle* th = get_tcp_handle(args[0]);
    // fault: lisp::TYPE_MISMATCH
    if (th == null || !th.connected || th.mode != TCP_STREAM) {
        return io_raise(interp, "io/tcp-read-invalid-stream-handle", "tcp-read: invalid or closed stream handle");
    }

    usz max_bytes = 0;
    long timeout_ms = -1;
    parse_tcp_read_options(args, &max_bytes, &timeout_ms);

    // Async bridge: when called from a scheduler-managed fiber, register a non-blocking
    // read with libuv and suspend this fiber until data is ready.
    Value* async_result = scheduler_try_async_tcp_read(th.fd, max_bytes, timeout_ms, interp);
    if (async_result != null) return async_result;

    char* buf = (char*)mem::malloc(max_bytes);
    // fault: lisp::READ_FAILED
    if (buf == null) return io_raise(interp, "io/tcp-read-out-of-memory", "tcp-read: out of memory");
    defer mem::free(buf);

    long received = c_recv(th.fd, buf, max_bytes, 0);
    if (received < 0) {
        // fault: lisp::READ_FAILED
        return io_raise(interp, "io/tcp-read-recv-failed", "tcp-read: recv failed");
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
// (tcp-close handle) — close connection
// ============================================================

fn Value* prim_tcp_close(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return io_raise(interp, "io/tcp-close-expected-handle", "tcp-close: expected (tcp-close handle)");

    TcpHandle* th = get_tcp_handle(args[0]);
    // fault: lisp::TYPE_MISMATCH
    if (th == null) return io_raise(interp, "io/tcp-close-invalid-handle", "tcp-close: invalid handle");

    if (th.connected) {
        c_close_fd(th.fd);
        th.connected = false;
        th.fd = -1;
    }

    return make_nil(interp);
}

// ============================================================
// UDP primitives
// ============================================================

// (udp-socket) — create an IPv4 UDP socket handle
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

    char[] path = args[0].str_chars[:args[0].str_len];
    char* path_heap = copy_to_heap_c_string(path);
    if (path_heap == null) {
        return io_raise(interp, "io/pipe-connect-out-of-memory", "pipe-connect: out of memory");
    }
    defer mem::free(path_heap);

    // Keep blocking-socket semantics as the primary path for language-level
    // `pipe-connect`; libuv setup stays as a fallback for environments where
    // direct Unix socket connect is unavailable.
    int fd = pipe_connect_fd(path);
    if (fd < 0) {
        int uv_status = omni_uv_pipe_connect_fd(path_heap, &fd);
        if (uv_status < 0 || fd < 0) {
            fd = -1;
        }
    }
    if (fd < 0) {
        return io_raise(interp, "io/pipe-connect-failed", "pipe-connect: connect failed");
    }
    return make_tcp_handle(interp, fd);
}

// (pipe-listen path) — create a Unix domain listener socket at path
fn Value* prim_pipe_listen(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return io_raise(interp, "io/pipe-listen-expected-path", "pipe-listen: expected (pipe-listen path)");
    }

    char[] path = args[0].str_chars[:args[0].str_len];
    char* path_heap = copy_to_heap_c_string(path);
    if (path_heap == null) {
        return io_raise(interp, "io/pipe-listen-out-of-memory", "pipe-listen: out of memory");
    }
    defer mem::free(path_heap);

    // Keep blocking-socket semantics as the primary path for language-level
    // `pipe-listen`; libuv setup stays as a fallback for environments where
    // direct Unix socket bind/listen is unavailable.
    int fd = pipe_listen_fd(path, 128);
    if (fd < 0) {
        int uv_status = omni_uv_pipe_listen_fd(path_heap, 128, &fd);
        if (uv_status < 0 || fd < 0) {
            fd = -1;
        }
    }
    if (fd < 0) {
        return io_raise(interp, "io/pipe-listen-failed", "pipe-listen: listen failed");
    }
    return make_tcp_listener_handle(interp, fd);
}

fn void close_process_handle(ProcessHandle* ph) {
    if (ph == null || !ph.open || ph.uv_process == null) return;
    omni_uv_process_close(ph.uv_process);
    ph.uv_process = null;
    ph.open = false;
}

// (process-spawn cmd args env) -> dict {'handle process-handle 'pid n 'stdin fs-handle 'stdout fs-handle 'stderr fs-handle}
fn Value* prim_process_spawn(Value*[] args, Env* env, Interp* interp) {
    Value* cmd_v = null;
    Value* argv_v = make_nil(interp);
    Value* env_v = make_nil(interp);

    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        cmd_v = car(packed);
        packed = cdr(packed);
        if (is_cons(packed)) {
            argv_v = car(packed);
            packed = cdr(packed);
        }
        if (is_cons(packed)) {
            env_v = car(packed);
            packed = cdr(packed);
        }
        if (!is_nil(packed)) {
            return io_raise(interp, "io/process-spawn-expected-cmd-args-env", "process-spawn: expected (process-spawn cmd args env)");
        }
    } else {
        if (args.len < 1 || args.len > 3) {
            return io_raise(interp, "io/process-spawn-expected-cmd-args-env", "process-spawn: expected (process-spawn cmd args env)");
        }
        cmd_v = args[0];
        if (args.len >= 2) argv_v = args[1];
        if (args.len >= 3) env_v = args[2];
    }

    if (!is_string(cmd_v)) {
        return io_raise(interp, "io/process-spawn-cmd-not-string", "process-spawn: cmd must be a string");
    }

    char* cmd_heap = copy_to_heap_c_string(cmd_v.str_chars[:cmd_v.str_len]);
    if (cmd_heap == null) {
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    defer mem::free(cmd_heap);

    char** argv_items = null;
    usz argv_item_count = 0;
    Value* argv_err = collect_string_sequence(
        argv_v,
        &argv_items,
        &argv_item_count,
        interp,
        "io/process-spawn-args-must-be-string-sequence",
        "process-spawn: args must be nil, list, or array of strings/symbols"
    );
    if (argv_err != null) return argv_err;
    defer free_heap_c_string_items(argv_items, argv_item_count);

    bool has_env_override = env_v != null && !is_nil(env_v);
    char** env_items = null;
    usz env_item_count = 0;
    if (has_env_override) {
        Value* env_err = collect_string_sequence(
            env_v,
            &env_items,
            &env_item_count,
            interp,
            "io/process-spawn-env-must-be-string-sequence",
            "process-spawn: env must be nil, list, or array of strings/symbols"
        );
        if (env_err != null) return env_err;
    }
    defer free_heap_c_string_items(env_items, env_item_count);

    char** argv_ptrs = (char**)mem::malloc(char*.sizeof * (argv_item_count + 2));
    if (argv_ptrs == null) {
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    defer mem::free(argv_ptrs);
    argv_ptrs[0] = cmd_heap;
    for (usz i = 0; i < argv_item_count; i++) {
        argv_ptrs[i + 1] = argv_items[i];
    }
    argv_ptrs[argv_item_count + 1] = null;

    char** env_ptrs = null;
    if (has_env_override) {
        env_ptrs = (char**)mem::malloc(char*.sizeof * (env_item_count + 1));
        if (env_ptrs == null) {
            return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
        }
        for (usz i = 0; i < env_item_count; i++) {
            env_ptrs[i] = env_items[i];
        }
        env_ptrs[env_item_count] = null;
    }
    defer if (env_ptrs != null) mem::free(env_ptrs);

    void* uv_process = null;
    int stdin_fd = -1;
    int stdout_fd = -1;
    int stderr_fd = -1;
    int spawn_status = omni_uv_process_spawn(
        cmd_heap,
        (void*)argv_ptrs,
        (void*)env_ptrs,
        &uv_process,
        &stdin_fd,
        &stdout_fd,
        &stderr_fd
    );
    if (spawn_status < 0 || uv_process == null) {
        if (stdin_fd >= 0) c_close_fd(stdin_fd);
        if (stdout_fd >= 0) c_close_fd(stdout_fd);
        if (stderr_fd >= 0) c_close_fd(stderr_fd);
        char[96] err_buf;
        char[] err_msg = io::bprintf(&err_buf, "process-spawn: spawn failed (%d)", spawn_status)!!;
        return io_raise(interp, "io/process-spawn-failed", err_msg);
    }

    Value* proc_handle_v = make_process_handle(interp, uv_process);
    if (proc_handle_v == null) {
        if (stdin_fd >= 0) c_close_fd(stdin_fd);
        if (stdout_fd >= 0) c_close_fd(stdout_fd);
        if (stderr_fd >= 0) c_close_fd(stderr_fd);
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }

    Value* stdin_h = make_fs_handle(interp, stdin_fd);
    if (stdin_h == null) {
        ProcessHandle* ph = get_process_handle(proc_handle_v);
        close_process_handle(ph);
        if (stdout_fd >= 0) c_close_fd(stdout_fd);
        if (stderr_fd >= 0) c_close_fd(stderr_fd);
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    Value* stdout_h = make_fs_handle(interp, stdout_fd);
    if (stdout_h == null) {
        close_fs_like_handle(stdin_h, interp);
        ProcessHandle* ph = get_process_handle(proc_handle_v);
        close_process_handle(ph);
        if (stderr_fd >= 0) c_close_fd(stderr_fd);
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    Value* stderr_h = make_fs_handle(interp, stderr_fd);
    if (stderr_h == null) {
        close_fs_like_handle(stdin_h, interp);
        close_fs_like_handle(stdout_h, interp);
        ProcessHandle* ph = get_process_handle(proc_handle_v);
        close_process_handle(ph);
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }

    ProcessHandle* ph = get_process_handle(proc_handle_v);
    int pid = (ph != null && ph.open && ph.uv_process != null) ? omni_uv_process_pid(ph.uv_process) : -1;

    Value* out = make_hashmap(interp, 16);
    Value* handle_key = make_symbol(interp, interp.symbols.intern("handle"));
    Value* pid_key = make_symbol(interp, interp.symbols.intern("pid"));
    Value* stdin_key = make_symbol(interp, interp.symbols.intern("stdin"));
    Value* stdout_key = make_symbol(interp, interp.symbols.intern("stdout"));
    Value* stderr_key = make_symbol(interp, interp.symbols.intern("stderr"));
    hashmap_set(out.hashmap_val, handle_key, proc_handle_v, interp);
    hashmap_set(out.hashmap_val, pid_key, make_int(interp, pid), interp);
    hashmap_set(out.hashmap_val, stdin_key, stdin_h, interp);
    hashmap_set(out.hashmap_val, stdout_key, stdout_h, interp);
    hashmap_set(out.hashmap_val, stderr_key, stderr_h, interp);
    return out;
}

// (process-wait process-handle|process-dict) -> dict {'pid n 'exit-code n 'term-signal n}
fn Value* prim_process_wait(Value*[] args, Env* env, Interp* interp) {
    Value* target_v = null;
    if (args.len == 1 && is_cons(args[0])) {
        target_v = car(args[0]);
    } else if (args.len >= 1) {
        target_v = args[0];
    } else {
        return io_raise(interp, "io/process-wait-expected-handle", "process-wait: expected (process-wait handle)");
    }

    Value* handle_v = value_to_process_handle_arg(target_v, interp);
    ProcessHandle* ph = get_process_handle(handle_v);
    if (ph == null || !ph.open || ph.uv_process == null) {
        return io_raise(interp, "io/process-wait-invalid-handle", "process-wait: invalid or closed process handle");
    }

    int pid = omni_uv_process_pid(ph.uv_process);
    int exit_code = 0;
    int term_signal = 0;
    int wait_status = omni_uv_process_wait(ph.uv_process, &exit_code, &term_signal);
    if (wait_status < 0) {
        return io_raise(interp, "io/process-wait-failed", "process-wait: wait failed");
    }

    close_process_handle(ph);

    Value* out = make_hashmap(interp, 8);
    Value* pid_key = make_symbol(interp, interp.symbols.intern("pid"));
    Value* exit_key = make_symbol(interp, interp.symbols.intern("exit-code"));
    Value* sig_key = make_symbol(interp, interp.symbols.intern("term-signal"));
    hashmap_set(out.hashmap_val, pid_key, make_int(interp, pid), interp);
    hashmap_set(out.hashmap_val, exit_key, make_int(interp, exit_code), interp);
    hashmap_set(out.hashmap_val, sig_key, make_int(interp, term_signal), interp);
    return out;
}

// (process-kill process-handle|process-dict sig) -> nil
fn Value* prim_process_kill(Value*[] args, Env* env, Interp* interp) {
    Value* target_v = null;
    Value* sig_v = null;
    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        target_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) {
            return io_raise(interp, "io/process-kill-expected-handle-sig", "process-kill: expected (process-kill handle sig)");
        }
        sig_v = car(packed);
    } else if (args.len >= 2) {
        target_v = args[0];
        sig_v = args[1];
    } else {
        return io_raise(interp, "io/process-kill-expected-handle-sig", "process-kill: expected (process-kill handle sig)");
    }

    if (!is_int(sig_v)) {
        return io_raise(interp, "io/process-kill-sig-not-integer", "process-kill: sig must be an integer");
    }

    Value* handle_v = value_to_process_handle_arg(target_v, interp);
    ProcessHandle* ph = get_process_handle(handle_v);
    if (ph == null || !ph.open || ph.uv_process == null) {
        return io_raise(interp, "io/process-kill-invalid-handle", "process-kill: invalid or closed process handle");
    }

    int sig = (int)sig_v.int_val;
    if (sig <= 0) sig = SIGKILL_VAL;
    int kill_status = omni_uv_process_kill(ph.uv_process, sig);
    if (kill_status < 0) {
        return io_raise(interp, "io/process-kill-failed", "process-kill: kill failed");
    }
    return make_nil(interp);
}

// (signal-handle sig callback) -> signal-handle
fn Value* prim_signal_handle(Value*[] args, Env* env, Interp* interp) {
    Value* sig_v = null;
    Value* callback_v = null;

    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        sig_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) {
            return io_raise(interp, "io/signal-handle-expected-sig-callback", "signal-handle: expected (signal-handle sig callback)");
        }
        callback_v = car(packed);
    } else if (args.len >= 2) {
        sig_v = args[0];
        callback_v = args[1];
    } else {
        return io_raise(interp, "io/signal-handle-expected-sig-callback", "signal-handle: expected (signal-handle sig callback)");
    }

    if (!is_int(sig_v)) {
        return io_raise(interp, "io/signal-handle-sig-not-integer", "signal-handle: sig must be an integer");
    }
    if (!signal_callback_callable(callback_v)) {
        return io_raise(interp, "io/signal-handle-callback-not-callable", "signal-handle: callback must be a procedure");
    }

    int signum = (int)sig_v.int_val;
    if (signum <= 0) {
        return io_raise(interp, "io/signal-handle-invalid-sig", "signal-handle: sig must be > 0");
    }

    void* uv_watch = null;
    int start_status = omni_uv_signal_start(signum, &uv_watch);
    if (start_status < 0 || uv_watch == null) {
        char[96] err_buf;
        char[] err_msg = io::bprintf(&err_buf, "signal-handle: failed to start (%d)", start_status)!!;
        return io_raise(interp, "io/signal-handle-failed", err_msg);
    }

    return make_signal_handle(interp, uv_watch, signum, callback_v);
}

// (signal-unhandle signal-handle) -> nil
fn Value* prim_signal_unhandle(Value*[] args, Env* env, Interp* interp) {
    Value* handle_v = null;
    if (args.len == 1 && is_cons(args[0])) {
        handle_v = car(args[0]);
    } else if (args.len >= 1) {
        handle_v = args[0];
    } else {
        return io_raise(interp, "io/signal-unhandle-expected-handle", "signal-unhandle: expected (signal-unhandle handle)");
    }

    SignalHandle* sh = get_signal_handle(handle_v);
    if (sh == null || !sh.open || sh.uv_signal_watch == null) {
        return io_raise(interp, "io/signal-unhandle-invalid-handle", "signal-unhandle: invalid or closed signal handle");
    }

    // Drain pending signals before teardown to preserve callback delivery.
    drain_signal_handle_pending(sh, interp);
    unregister_signal_handle(sh);
    close_signal_handle(sh);
    return make_nil(interp);
}

// ============================================================
// (dns-resolve hostname) — resolve to IP string
// ============================================================

fn Value* prim_dns_resolve(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return io_raise(interp, "io/dns-resolve-expected-hostname", "dns-resolve: expected (dns-resolve hostname)");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[0])) return io_raise(interp, "io/dns-resolve-hostname-not-string", "dns-resolve: hostname must be a string");

    char[] host = args[0].str_chars[:args[0].str_len];

    // Fiber path: use libuv async resolver and suspend until callback completion.
    if (scheduler_is_running_fiber_context() && g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
        usz fiber_id = g_scheduler.current_fiber_id;
        PendingDnsResolve* dns = &g_scheduler.pending_dns[fiber_id];
        if (dns.active) {
            return io_raise(
                interp,
                "io/dns-resolve-pending-request-exists",
                "dns-resolve: pending request already exists for this fiber"
            );
        }

        char* host_copy = copy_to_heap_c_string(host);
        if (host_copy == null) {
            return io_raise(interp, "io/dns-resolve-out-of-memory", "dns-resolve: out of memory");
        }

        void* req_handle = omni_uv_getaddrinfo_req_new();
        if (req_handle == null) {
            mem::free(host_copy);
            return io_raise(interp, "io/dns-resolve-out-of-memory", "dns-resolve: out of memory");
        }

        omni_uv_req_set_data(req_handle, host_copy);

        dns.active = true;
        dns.completed = false;
        dns.status = 0;
        dns.req_handle = req_handle;
        dns.addrinfo_result = null;
        dns.host_copy = host_copy;

        int start_status = omni_uv_getaddrinfo_start(
            g_scheduler.uv_loop,
            req_handle,
            &scheduler_uv_getaddrinfo_cb,
            host_copy,
            null
        );
        if (start_status != 0) {
            omni_uv_req_set_data(req_handle, null);
            omni_uv_getaddrinfo_req_free(req_handle);
            mem::free(host_copy);
            g_scheduler.pending_dns[fiber_id] = {};
            return io_raise(interp, "io/dns-resolve-async-init-failed", "dns-resolve: async resolver init failed");
        }

        g_scheduler.fibers[fiber_id].state = FIBER_BLOCKED;

        Value*[1] yargs;
        yargs[0] = make_nil(interp);
        Value* y = prim_yield(yargs[..], null, interp);
        if (y != null && y.tag == ERROR) return y;

        if (!dns.active || !dns.completed || dns.req_handle == null) {
            return io_raise(
                interp,
                "io/dns-resolve-resumed-before-completion",
                "dns-resolve: resumed before completion"
            );
        }

        void* result = dns.addrinfo_result;
        int status = dns.status;
        void* done_req = dns.req_handle;
        g_scheduler.pending_dns[fiber_id] = {};

        if (status != 0 || result == null) {
            if (result != null) omni_uv_freeaddrinfo(result);
            omni_uv_getaddrinfo_req_free(done_req);
            return io_raise(interp, "io/dns-resolve-resolution-failed", "dns-resolve: resolution failed");
        }

        Value* resolved = dns_result_to_ipv4_string(interp, result);
        omni_uv_freeaddrinfo(result);
        omni_uv_getaddrinfo_req_free(done_req);
        return resolved;
    }

    char[256] host_buf;  // buffer: C-interop (keep) — null-terminated for c_getaddrinfo
    copy_to_c_buffer(&host_buf, host_buf.len, host);

    void* result = null;
    int status = c_getaddrinfo(&host_buf, null, null, &result);
    if (status != 0 || result == null) {
        // fault: lisp::DNS_FAILED
        return io_raise(interp, "io/dns-resolve-resolution-failed", "dns-resolve: resolution failed");
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
    if (args.len < 1) return io_raise(interp, "io/async-sleep-expected-ms", "async-sleep: expected (async-sleep ms)");
    // fault: lisp::EXPECTED_INT
    if (!is_int(args[0])) return io_raise(interp, "io/async-sleep-ms-not-integer", "async-sleep: ms must be an integer");

    long ms = args[0].int_val;
    if (ms <= 0) return make_nil(interp);

    // In fiber context: prefer libuv timer and suspend current fiber until timer callback.
    if (scheduler_is_running_fiber_context()) {
        if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
            usz fiber_id = g_scheduler.current_fiber_id;
            PendingSleep* sleep = &g_scheduler.pending_sleeps[fiber_id];
            if (sleep.active) {
                return io_raise(
                    interp,
                    "io/async-sleep-pending-timer-exists",
                    "async-sleep: pending timer already exists for this fiber"
                );
            }

            usz timer_size = uv_handle_size(UV_HANDLE_TIMER);
            if (timer_size > 0) {
                void* timer_handle = mem::malloc(timer_size);
                if (timer_handle == null) {
                    return io_raise(interp, "io/async-sleep-out-of-memory", "async-sleep: out of memory");
                }
                if (uv_timer_init(g_scheduler.uv_loop, timer_handle) == 0) {
                    sleep.active = true;
                    sleep.completed = false;
                    sleep.timer_handle = timer_handle;

                    if (uv_timer_start(timer_handle, &scheduler_uv_sleep_timer_cb, (ulong)ms, 0) == 0) {
                        g_scheduler.fibers[fiber_id].state = FIBER_BLOCKED;

                        Value*[1] yargs;
                        yargs[0] = make_nil(interp);
                        Value* y = prim_yield(yargs[..], null, interp);
                        if (y != null && y.tag == ERROR) return y;
                        return scheduler_consume_pending_sleep(fiber_id, interp);
                    }

                    // Timer start failed; release handle and clear pending slot.
                    uv_close(timer_handle, &scheduler_free_handle_cb);
                    g_scheduler.pending_sleeps[fiber_id] = {};
                } else {
                    mem::free(timer_handle);
                }
            }
        }

        // Fallback: worker offload path if libuv timer setup is unavailable.
        Value* op = make_symbol(interp, interp.symbols.intern("sleep-ms"));
        Value* job = make_cons(interp, op, make_cons(interp, args[0], make_nil(interp)));
        Value*[1] off_args;
        off_args[0] = job;
        return prim_offload(off_args[..], null, interp);
    }

    c_usleep((uint)(ms * 1000));
    return make_nil(interp);
}
