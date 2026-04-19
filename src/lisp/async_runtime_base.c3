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
alias UvWorkCb = fn void(void* req);
alias UvAfterWorkCb = fn void(void* req, int status);
alias UvThreadEntryCb = fn void(void* arg);
alias UvConnectCb = fn void(void* req, int status);
alias UvConnectionCb = fn void(void* server_handle, int status);
alias UvTimerShimCallback = fn void(void* user_data);
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
extern fn void* omni_uv_work_req_new() @extern("omni_uv_work_req_new");
extern fn void omni_uv_work_req_free(void* req) @extern("omni_uv_work_req_free");
extern fn int omni_uv_queue_work_start(void* loop, void* req, UvWorkCb work_cb, UvAfterWorkCb after_cb) @extern("omni_uv_queue_work_start");
extern fn void* omni_uv_thread_new() @extern("omni_uv_thread_new");
extern fn void omni_uv_thread_free(void* thread_handle) @extern("omni_uv_thread_free");
extern fn int omni_uv_thread_start(void* thread_handle, UvThreadEntryCb entry, void* arg) @extern("omni_uv_thread_start");
extern fn int omni_uv_thread_join_handle(void* thread_handle) @extern("omni_uv_thread_join_handle");
extern fn int omni_uv_thread_detach_handle(void* thread_handle) @extern("omni_uv_thread_detach_handle");
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
extern fn int omni_uv_process_poll(
    void* process,
    int* completed_out,
    int* exit_status_out,
    int* term_signal_out
) @extern("omni_uv_process_poll");
extern fn int omni_uv_process_wait(void* process, int* exit_status_out, int* term_signal_out) @extern("omni_uv_process_wait");
extern fn int omni_uv_process_kill(void* process, int sig) @extern("omni_uv_process_kill");
extern fn int omni_uv_process_pid(void* process) @extern("omni_uv_process_pid");
extern fn void omni_uv_process_close(void* process) @extern("omni_uv_process_close");
extern fn void omni_uv_process_debug_reset_counters() @extern("omni_uv_process_debug_reset_counters");
extern fn int omni_uv_process_debug_poll_calls() @extern("omni_uv_process_debug_poll_calls");
extern fn int omni_uv_process_debug_wait_calls() @extern("omni_uv_process_debug_wait_calls");
extern fn int omni_uv_signal_start(int signum, void** out_watch) @extern("omni_uv_signal_start");
extern fn int omni_uv_signal_poll_pending(void* signal_watch) @extern("omni_uv_signal_poll_pending");
extern fn int omni_uv_signal_stop(void* signal_watch) @extern("omni_uv_signal_stop");
extern fn int omni_uv_signal_debug_set_pending(void* signal_watch, int pending_count) @extern("omni_uv_signal_debug_set_pending");
extern fn int omni_uv_signal_debug_pending(void* signal_watch) @extern("omni_uv_signal_debug_pending");
extern fn int omni_uv_timer_callback_register(
    UvTimerShimCallback callback,
    void* user_data,
    void** out_handle
) @extern("omni_uv_timer_callback_register");
extern fn int omni_uv_timer_callback_invoke(void* callback_handle) @extern("omni_uv_timer_callback_invoke");
extern fn int omni_uv_timer_callback_unregister(void* callback_handle) @extern("omni_uv_timer_callback_unregister");
extern fn void omni_uv_timer_callback_free(void* callback_handle) @extern("omni_uv_timer_callback_free");
extern fn int omni_uv_timer_callback_debug_is_open(void* callback_handle) @extern("omni_uv_timer_callback_debug_is_open");
extern fn int omni_uv_timer_callback_debug_invoke_count(void* callback_handle) @extern("omni_uv_timer_callback_debug_invoke_count");
extern fn int omni_uv_pipe_connect_start(
    void* loop,
    char* path,
    UvConnectCb cb,
    void** out_req,
    void** out_pipe_handle
) @extern("omni_uv_pipe_connect_start");
extern fn int omni_uv_pipe_listen_start(void* loop, char* path, int backlog, void** out_server_handle, int* out_fd) @extern("omni_uv_pipe_listen_start");
extern fn int omni_uv_pipe_accept_prepare(
    void* loop,
    void* listener_handle,
    UvConnectionCb cb,
    void** out_client_handle
) @extern("omni_uv_pipe_accept_prepare");
extern fn int omni_uv_pipe_listener_set_noop(void* listener_handle) @extern("omni_uv_pipe_listener_set_noop");
extern fn int omni_uv_tcp_connect_start(
    void* loop,
    char* ip,
    int port,
    UvConnectCb cb,
    void** out_req,
    void** out_tcp_handle
) @extern("omni_uv_tcp_connect_start");
extern fn void omni_uv_tcp_connect_req_free(void* req) @extern("omni_uv_tcp_connect_req_free");
extern fn void omni_uv_tcp_close_and_free(void* tcp_handle) @extern("omni_uv_tcp_close_and_free");
extern fn int omni_uv_tcp_connect_detach_fd(void* tcp_handle, int* out_fd) @extern("omni_uv_tcp_connect_detach_fd");
extern fn int omni_uv_tcp_listen_fd(char* ip, int port, int backlog, int* out_fd) @extern("omni_uv_tcp_listen_fd");
extern fn int omni_uv_tcp_accept_start(
    void* loop,
    int listener_fd,
    UvConnectionCb cb,
    void** out_server_handle,
    void** out_client_handle
) @extern("omni_uv_tcp_accept_start");
extern fn int omni_uv_tcp_accept_detach_fd(void* server_handle, void* client_handle, int* out_fd) @extern("omni_uv_tcp_accept_detach_fd");
extern fn int omni_uv_tcp_close_fd(int fd) @extern("omni_uv_tcp_close_fd");
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

// addrinfo result — we only need ai_family, ai_addrlen, ai_addr, ai_next
// offsets vary by platform, so we read fields via helpers
const int AF_INET = 2;
const int AF_INET6 @if(env::DARWIN) = 30;
const int AF_INET6 @if(!env::DARWIN) = 10;
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

enum TcpTransport : char {
    TCP_TRANSPORT_INET,
    TCP_TRANSPORT_PIPE
}

struct TcpHandle {
    int fd;
    bool connected;
    TcpHandleMode mode;
    TcpTransport transport;
    void* uv_listener_handle; // persistent listener handle for libuv pipe listeners
}

struct UdpHandle {
    int fd;
    bool open;
}

fn Value* make_tcp_listener_handle_with_transport(
    Interp* interp,
    int fd,
    TcpTransport transport = TCP_TRANSPORT_INET,
    void* uv_listener_handle = null
) {
    TcpHandle* th = (TcpHandle*)mem::malloc(TcpHandle.sizeof);
    if (th == null) {
        if (fd >= 0) {
            c_close_fd(fd);
        }
        if (uv_listener_handle != null) {
            omni_uv_tcp_close_and_free(uv_listener_handle);
        }
        return io_raise(interp, "io/tcp-listen-out-of-memory", "tcp-listen: out of memory");
    }
    th.fd = fd;
    th.connected = true;
    th.mode = TCP_LISTENER;
    th.transport = transport;
    th.uv_listener_handle = uv_listener_handle;
    Value* v = make_ffi_handle_ex(interp, (void*)th, "tcp-handle",
        &tcp_handle_finalizer, true, false);
    if (v == null) {
        return io_raise(interp, "io/tcp-listen-out-of-memory", "tcp-listen: out of memory");
    }
    return v;
}

fn Value* make_tcp_handle(Interp* interp, int fd) @inline {
    return make_tcp_stream_handle(interp, fd, TCP_TRANSPORT_INET);
}

fn Value* make_pipe_handle(Interp* interp, int fd) @inline {
    return make_tcp_stream_handle(interp, fd, TCP_TRANSPORT_PIPE);
}

fn Value* make_tcp_listener_handle(Interp* interp, int fd) @inline {
    return make_tcp_listener_handle_with_transport(interp, fd, TCP_TRANSPORT_INET);
}

fn Value* make_pipe_listener_handle(Interp* interp, int fd, void* uv_listener_handle = null) @inline {
    return make_tcp_listener_handle_with_transport(interp, fd, TCP_TRANSPORT_PIPE, uv_listener_handle);
}

fn void copy_to_c_buffer(char* dst, usz cap, char[] src) {
    if (cap == 0) return;
    usz n = src.len < (cap - 1) ? src.len : (cap - 1);
    for (usz i = 0; i < n; i++) dst[i] = src[i];
    dst[n] = 0;
}
