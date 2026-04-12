module lisp;

import std::core::mem;

fn void tcp_handle_finalizer(void* handle) {
    TcpHandle* th = (TcpHandle*)handle;
    if (th == null) return;
    if (th.uv_listener_handle != null) {
        omni_uv_tcp_close_and_free(th.uv_listener_handle);
        th.uv_listener_handle = null;
    }
    if (th.connected && th.fd >= 0) {
        c_close_fd(th.fd);
        th.connected = false;
        th.fd = -1;
    }
}

fn Value* make_tcp_stream_handle(Interp* interp, int fd, TcpTransport transport = TCP_TRANSPORT_INET) {
    TcpHandle* th = (TcpHandle*)mem::malloc(TcpHandle.sizeof);
    if (th == null) return io_raise(interp, "io/tcp-connect-out-of-memory", "tcp-connect: out of memory");
    th.fd = fd;
    th.connected = true;
    th.mode = TCP_STREAM;
    th.transport = transport;
    th.uv_listener_handle = null;
    bool ctor_took_ownership = false;
    Value* v = make_ffi_handle_ex(interp, (void*)th, "tcp-handle",
        &tcp_handle_finalizer, true, false, &ctor_took_ownership);
    if (v == null) {
        if (!ctor_took_ownership) {
            if (th.fd >= 0) c_close_fd(th.fd);
            mem::free(th);
        }
        return io_raise(interp, "io/tcp-connect-out-of-memory", "tcp-connect: out of memory");
    }
    return v;
}

fn TcpHandle* get_tcp_handle(Value* v) {
    if (!ffi_handle_name_equals(v, "tcp-handle")) return null;
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
    bool ctor_took_ownership = false;
    Value* v = make_ffi_handle_ex(interp, (void*)uh, "udp-handle",
        &udp_handle_finalizer, true, false, &ctor_took_ownership);
    if (v == null) {
        if (!ctor_took_ownership) {
            mem::free(uh);
            c_close_fd(fd);
        }
        return io_raise(interp, "io/udp-socket-out-of-memory", "udp-socket: out of memory");
    }
    return v;
}

fn UdpHandle* get_udp_handle(Value* v) {
    if (!ffi_handle_name_equals(v, "udp-handle")) return null;
    return (UdpHandle*)v.ffi_val.lib_handle;
}
