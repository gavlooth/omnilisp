module lisp;

import std::core::mem;

// ============================================================
// (tcp-connect host port) — async connect in fiber context, returns handle
// ============================================================

<*
@require interp != null
*>
fn Value* prim_tcp_connect(Value*[] args, Env* env, Interp* interp) {
    Value* host_v = null;
    Value* port_v = null;
    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        if (is_nil(cdr(packed)) && is_cons(car(packed))) {
            packed = car(packed);
        }
        host_v = car(packed);
        packed = cdr(packed);
        if (is_cons(packed)) {
            port_v = car(packed);
            packed = cdr(packed);
            if (!is_nil(packed)) {
                return io_raise(interp, "io/tcp-connect-expected-host-port", "tcp-connect: expected (tcp-connect host port)");
            }
        } else if (!is_nil(packed)) {
            port_v = packed;
        } else {
            return io_raise(interp, "io/tcp-connect-expected-host-port", "tcp-connect: expected (tcp-connect host port)");
        }
    } else if (args.len == 2) {
        host_v = args[0];
        port_v = args[1];
    } else {
        return io_raise(interp, "io/tcp-connect-expected-host-port", "tcp-connect: expected (tcp-connect host port)");
    }

    // fault: lisp::EXPECTED_STRING
    if (!is_string(host_v)) return io_raise(interp, "io/tcp-connect-host-not-string", "tcp-connect: host must be a string");
    // fault: lisp::EXPECTED_INT
    if (!is_int(port_v)) return io_raise(interp, "io/tcp-connect-port-not-integer", "tcp-connect: port must be an integer");

    long raw_port = port_v.int_val;
    if (raw_port < 0 || raw_port > 65535) {
        return io_raise(interp, "io/tcp-connect-invalid-port", "tcp-connect: port must be between 0 and 65535");
    }
    int port = (int)raw_port;

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
    dns_args[0] = host_v;
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

    PendingConnectCallbackCtx* req_ctx = (PendingConnectCallbackCtx*)mem::malloc(PendingConnectCallbackCtx.sizeof);
    if (req_ctx == null) {
        omni_uv_tcp_connect_req_free(req_handle);
        omni_uv_tcp_close_and_free(tcp_handle);
        return io_raise(interp, "io/tcp-connect-out-of-memory", "tcp-connect: out of memory");
    }
    *req_ctx = {};
    req_ctx.fiber_id = fiber_id;
    req_ctx.request_token = scheduler_next_async_request_token();
    req_ctx.tcp_handle = tcp_handle;
    omni_uv_req_set_data(req_handle, req_ctx);

    pending.active = true;
    pending.completed = false;
    pending.status = 0;
    pending.request_token = req_ctx.request_token;
    pending.req_handle = req_handle;
    pending.tcp_handle = tcp_handle;

    g_scheduler.fibers[fiber_id].state = FIBER_BLOCKED;

    Value*[1] yargs;
    yargs[0] = make_nil(interp);
    Value* y = prim_yield(yargs[..], null, interp);
    if (y != null && y.tag == ERROR) {
        scheduler_close_pending_tcp_connect(fiber_id);
        return y;
    }

    if (!pending.active ||
        !pending.completed ||
        pending.request_token == 0 ||
        pending.req_handle == null ||
        pending.tcp_handle == null) {
        scheduler_close_pending_tcp_connect(fiber_id);
        return io_raise(
            interp,
            "io/tcp-connect-resumed-before-completion",
            "tcp-connect: resumed before connect completion"
        );
    }

    int status = pending.status;
    void* done_req = pending.req_handle;
    void* done_tcp = pending.tcp_handle;
    PendingConnectCallbackCtx* done_ctx = scheduler_take_connect_callback_ctx(done_req);
    g_scheduler.pending_connects[fiber_id] = {};

    if (status != 0) {
        scheduler_free_connect_callback_ctx(done_ctx);
        omni_uv_tcp_connect_req_free(done_req);
        omni_uv_tcp_close_and_free(done_tcp);
        return io_raise(interp, "io/tcp-connect-connection-failed", "tcp-connect: connection failed");
    }

    int fd = -1;
    int fd_status = omni_uv_tcp_connect_detach_fd(done_tcp, &fd);
    scheduler_free_connect_callback_ctx(done_ctx);
    omni_uv_tcp_connect_req_free(done_req);
    omni_uv_tcp_close_and_free(done_tcp);
    if (fd_status != 0 || fd < 0) {
        return io_raise(interp, "io/tcp-connect-fd-detach-failed", "tcp-connect: failed to detach socket");
    }

    return make_tcp_handle(interp, fd);
}
