module lisp;

import std::core::mem;

fn void scheduler_uv_timer_cb(void* handle) {
    isz idx = scheduler_find_pending_by_handle(handle);
    if (idx < 0) return;

    PendingTcpRead* op = &g_scheduler.pending_reads[(usz)idx];
    if (!op.active || op.completed) return;

    if (!scheduler_publish_timer_expired((usz)idx)) {
        scheduler_handle_wakeup_timer_expired((usz)idx);
    }
}

fn void scheduler_uv_sleep_timer_cb(void* handle) {
    isz idx = scheduler_find_pending_sleep_by_handle(handle);
    if (idx < 0) return;
    PendingSleep* sleep = &g_scheduler.pending_sleeps[(usz)idx];
    if (!sleep.active || sleep.completed) return;
    if (!scheduler_publish_sleep_done((usz)idx)) {
        scheduler_handle_wakeup_sleep_done((usz)idx);
    }
}

fn void scheduler_uv_getaddrinfo_cb(void* req, int status, void* res) {
    PendingDnsCallbackCtx* ctx = scheduler_take_dns_callback_ctx(req);
    usz fiber_id = NO_FIBER;
    uint request_token = 0;
    if (ctx != null) {
        fiber_id = ctx.fiber_id;
        request_token = ctx.request_token;
        if (ctx.host_copy != null) {
            mem::free(ctx.host_copy);
            ctx.host_copy = null;
        }
    }

    if (fiber_id >= g_scheduler.fiber_count) {
        if (res != null) omni_uv_freeaddrinfo(res);
        scheduler_free_dns_callback_ctx(ctx);
        omni_uv_getaddrinfo_req_free(req);
        return;
    }

    PendingDnsResolve* dns = &g_scheduler.pending_dns[fiber_id];
    if (dns.req_handle != req || dns.request_token == 0 || dns.request_token != request_token) {
        if (res != null) omni_uv_freeaddrinfo(res);
        scheduler_free_dns_callback_ctx(ctx);
        omni_uv_getaddrinfo_req_free(req);
        return;
    }

    dns.host_copy = null;

    if (!dns.active || dns.completed) {
        if (res != null) omni_uv_freeaddrinfo(res);
        scheduler_free_dns_callback_ctx(ctx);
        omni_uv_getaddrinfo_req_free(req);
        dns.req_handle = null;
        dns.addrinfo_result = null;
        dns.completed = false;
        dns.status = status;
        dns.request_token = 0;
        return;
    }

    if (ctx != null) omni_uv_req_set_data(req, ctx);
    dns.status = status;
    dns.addrinfo_result = res;
    dns.completed = true;
    if (!scheduler_publish_dns_done(fiber_id)) {
        scheduler_complete_pending_dns(fiber_id);
    }
}

fn void scheduler_uv_tcp_connect_cb(void* req, int status) {
    PendingConnectCallbackCtx* ctx = scheduler_take_connect_callback_ctx(req);
    usz fiber_id = NO_FIBER;
    uint request_token = 0;
    void* tcp_handle = null;
    if (ctx != null) {
        fiber_id = ctx.fiber_id;
        request_token = ctx.request_token;
        tcp_handle = ctx.tcp_handle;
    }

    if (fiber_id >= g_scheduler.fiber_count) {
        if (tcp_handle != null) omni_uv_tcp_close_and_free(tcp_handle);
        scheduler_free_connect_callback_ctx(ctx);
        omni_uv_tcp_connect_req_free(req);
        return;
    }

    PendingTcpConnect* pending = &g_scheduler.pending_connects[fiber_id];
    if (pending.req_handle != req || pending.request_token == 0 || pending.request_token != request_token) {
        if (tcp_handle != null) omni_uv_tcp_close_and_free(tcp_handle);
        scheduler_free_connect_callback_ctx(ctx);
        omni_uv_tcp_connect_req_free(req);
        return;
    }

    pending.status = status;
    pending.completed = true;

    if (!pending.active || pending.req_handle == null || pending.tcp_handle == null) {
        if (pending.req_handle != null) {
            omni_uv_tcp_connect_req_free(pending.req_handle);
        }
        if (pending.tcp_handle != null) {
            omni_uv_tcp_close_and_free(pending.tcp_handle);
        }
        if (ctx != null && ctx.tcp_handle != null && ctx.tcp_handle != pending.tcp_handle) {
            omni_uv_tcp_close_and_free(ctx.tcp_handle);
        }
        scheduler_free_connect_callback_ctx(ctx);
        g_scheduler.pending_connects[fiber_id] = {};
        return;
    }

    if (ctx != null) omni_uv_req_set_data(req, ctx);
    if (!scheduler_publish_tcp_connect_done(fiber_id)) {
        scheduler_complete_pending_tcp_connect(fiber_id);
    }
}

fn void scheduler_uv_tcp_accept_cb(void* server_handle, int status) {
    isz idx = scheduler_find_pending_tcp_accept_by_server(server_handle);
    if (idx < 0) {
        return;
    }

    PendingTcpAccept* pending = &g_scheduler.pending_accepts[(usz)idx];
    if (!pending.active ||
        pending.completed ||
        pending.server_handle == null ||
        pending.client_handle == null) {
        scheduler_close_pending_tcp_accept((usz)idx);
        return;
    }

    int accepted_fd = -1;
    int accept_status = status;
    if (status >= 0) {
        accept_status = omni_uv_tcp_accept_detach_fd(pending.server_handle, pending.client_handle, &accepted_fd);
    }

    void* done_server = pending.server_handle;
    void* done_client = pending.client_handle;
    bool borrowed_done_server = pending.borrowed_server_handle;
    pending.server_handle = null;
    pending.client_handle = null;
    if (done_server != null && borrowed_done_server) {
        (void)omni_uv_pipe_listener_set_noop(done_server);
    }
    if (done_server != null && !borrowed_done_server) {
        omni_uv_tcp_close_and_free(done_server);
    }
    if (done_client != null) {
        omni_uv_tcp_close_and_free(done_client);
    }

    pending.status = accept_status;
    pending.accepted_fd = accepted_fd;
    pending.has_accepted_fd = accepted_fd >= 0;
    pending.completed = true;

    if (!scheduler_publish_tcp_accept_done((usz)idx)) {
        scheduler_complete_pending_tcp_accept((usz)idx);
    }
}

fn void scheduler_uv_poll_cb(void* handle, int status, int events) {
    isz idx = scheduler_find_pending_by_handle(handle);
    if (idx < 0) return;

    PendingTcpRead* op = &g_scheduler.pending_reads[(usz)idx];
    if (!op.active || op.completed) return;

    if (status < 0) {
        if (!scheduler_publish_poll_error((usz)idx, status)) {
            scheduler_handle_wakeup_poll_error((usz)idx, status);
        }
    } else {
        if ((events & UV_READABLE) != 0) {
            scheduler_signal_poll_readable((usz)idx);
        }
        if ((events & UV_WRITABLE) != 0) {
            scheduler_signal_poll_writable((usz)idx);
        }
    }
}
