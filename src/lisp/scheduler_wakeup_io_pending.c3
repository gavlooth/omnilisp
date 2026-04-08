module lisp;

import std::core::mem;
import main;

fn uint scheduler_next_async_request_token() {
    g_scheduler.async_request_token_seed++;
    if (g_scheduler.async_request_token_seed == 0) g_scheduler.async_request_token_seed = 1;
    return g_scheduler.async_request_token_seed;
}

fn PendingDnsCallbackCtx* scheduler_take_dns_callback_ctx(void* req_handle) {
    if (req_handle == null) return null;
    PendingDnsCallbackCtx* ctx = (PendingDnsCallbackCtx*)omni_uv_req_get_data(req_handle);
    if (ctx != null) {
        omni_uv_req_set_data(req_handle, null);
    }
    return ctx;
}

fn void scheduler_free_dns_callback_ctx(PendingDnsCallbackCtx* ctx) {
    if (ctx == null) return;
    if (ctx.host_copy != null) {
        mem::free(ctx.host_copy);
        ctx.host_copy = null;
    }
    mem::free(ctx);
}

fn PendingConnectCallbackCtx* scheduler_take_connect_callback_ctx(void* req_handle) {
    if (req_handle == null) return null;
    PendingConnectCallbackCtx* ctx = (PendingConnectCallbackCtx*)omni_uv_req_get_data(req_handle);
    if (ctx != null) {
        omni_uv_req_set_data(req_handle, null);
    }
    return ctx;
}

fn void scheduler_free_connect_callback_ctx(PendingConnectCallbackCtx* ctx) {
    if (ctx == null) return;
    mem::free(ctx);
}

fn isz scheduler_find_pending_by_handle(void* handle) {
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        PendingTcpRead* op = &g_scheduler.pending_reads[i];
        if (!op.active) continue;
        if (op.poll_handle == handle || op.timer_handle == handle) return (isz)i;
    }
    return -1;
}

fn isz scheduler_find_pending_sleep_by_handle(void* handle) {
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        PendingSleep* sleep = &g_scheduler.pending_sleeps[i];
        if (!sleep.active) continue;
        if (sleep.timer_handle == handle) return (isz)i;
    }
    return -1;
}

fn isz scheduler_find_pending_dns_by_req(void* req_handle) {
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        PendingDnsResolve* dns = &g_scheduler.pending_dns[i];
        if (dns.req_handle == req_handle) return (isz)i;
    }
    return -1;
}

fn isz scheduler_find_pending_tcp_connect_by_req(void* req_handle) {
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        PendingTcpConnect* pending = &g_scheduler.pending_connects[i];
        if (pending.req_handle == req_handle) return (isz)i;
    }
    return -1;
}

fn isz scheduler_find_pending_tcp_accept_by_server(void* server_handle) {
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        PendingTcpAccept* pending = &g_scheduler.pending_accepts[i];
        if (pending.server_handle == server_handle) return (isz)i;
    }
    return -1;
}
