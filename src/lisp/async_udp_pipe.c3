module lisp;

import std::core::mem;
import std::io;
import main;

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

    PendingConnectCallbackCtx* req_ctx = (PendingConnectCallbackCtx*)mem::malloc(PendingConnectCallbackCtx.sizeof);
    if (req_ctx == null) {
        omni_uv_tcp_connect_req_free(req_handle);
        omni_uv_tcp_close_and_free(pipe_handle);
        return io_raise(interp, "io/pipe-connect-out-of-memory", "pipe-connect: out of memory");
    }
    *req_ctx = {};
    req_ctx.fiber_id = fiber_id;
    req_ctx.request_token = scheduler_next_async_request_token();
    req_ctx.tcp_handle = pipe_handle;
    omni_uv_req_set_data(req_handle, req_ctx);

    pending.active = true;
    pending.completed = false;
    pending.status = 0;
    pending.request_token = req_ctx.request_token;
    pending.req_handle = req_handle;
    pending.tcp_handle = pipe_handle;

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
        return io_raise(
            interp,
            "io/pipe-connect-resumed-before-completion",
            "pipe-connect: resumed before connect completion"
        );
    }

    int status = pending.status;
    void* done_req = pending.req_handle;
    void* done_pipe = pending.tcp_handle;
    PendingConnectCallbackCtx* done_ctx = scheduler_take_connect_callback_ctx(done_req);
    g_scheduler.pending_connects[fiber_id] = {};

    if (status != 0) {
        scheduler_free_connect_callback_ctx(done_ctx);
        omni_uv_tcp_connect_req_free(done_req);
        omni_uv_tcp_close_and_free(done_pipe);
        return io_raise(interp, "io/pipe-connect-failed", "pipe-connect: connect failed");
    }

    int fd = -1;
    int fd_status = omni_uv_tcp_connect_detach_fd(done_pipe, &fd);
    scheduler_free_connect_callback_ctx(done_ctx);
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
