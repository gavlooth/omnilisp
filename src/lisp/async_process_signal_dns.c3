module lisp;

import std::io;
import main;

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

// (signal-unhandle signal-handle) -> void
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
    signal_handle_detach_runtime_state(sh);
    return make_void(interp);
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

    if (!scheduler_is_running_fiber_context()) {
        return io_raise(
            interp,
            "io/dns-resolve-fiber-required",
            "dns-resolve: requires running fiber context"
        );
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return io_raise(
            interp,
            "io/dns-resolve-loop-unavailable",
            "dns-resolve: libuv loop unavailable"
        );
    }

    // Keep localhost deterministic across host/container resolver differences.
    if (is_localhost_name(host)) {
        return make_string(interp, "127.0.0.1");
    }

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

    PendingDnsCallbackCtx* req_ctx = (PendingDnsCallbackCtx*)mem::malloc(PendingDnsCallbackCtx.sizeof);
    if (req_ctx == null) {
        omni_uv_getaddrinfo_req_free(req_handle);
        mem::free(host_copy);
        return io_raise(interp, "io/dns-resolve-out-of-memory", "dns-resolve: out of memory");
    }
    *req_ctx = {};
    req_ctx.fiber_id = fiber_id;
    req_ctx.request_token = scheduler_next_async_request_token();
    req_ctx.host_copy = host_copy;
    omni_uv_req_set_data(req_handle, req_ctx);

    dns.active = true;
    dns.completed = false;
    dns.status = 0;
    dns.request_token = req_ctx.request_token;
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
        scheduler_free_dns_callback_ctx(req_ctx);
        omni_uv_getaddrinfo_req_free(req_handle);
        g_scheduler.pending_dns[fiber_id] = {};
        return io_raise(interp, "io/dns-resolve-async-init-failed", "dns-resolve: async resolver init failed");
    }

    g_scheduler.fibers[fiber_id].state = FIBER_BLOCKED;

    Value*[1] yargs;
    yargs[0] = make_nil(interp);
    Value* y = prim_yield(yargs[..], null, interp);
    if (y != null && y.tag == ERROR) return y;

    if (!dns.active || !dns.completed || dns.req_handle == null || dns.request_token == 0) {
        return io_raise(
            interp,
            "io/dns-resolve-resumed-before-completion",
            "dns-resolve: resumed before completion"
        );
    }

    void* result = dns.addrinfo_result;
    int status = dns.status;
    void* done_req = dns.req_handle;
    PendingDnsCallbackCtx* done_ctx = scheduler_take_dns_callback_ctx(done_req);
    g_scheduler.pending_dns[fiber_id] = {};

    if (status != 0 || result == null) {
        if (result != null) omni_uv_freeaddrinfo(result);
        scheduler_free_dns_callback_ctx(done_ctx);
        omni_uv_getaddrinfo_req_free(done_req);
        return io_raise(interp, "io/dns-resolve-resolution-failed", "dns-resolve: resolution failed");
    }

    Value* resolved = dns_result_to_ipv4_string(interp, result);
    omni_uv_freeaddrinfo(result);
    scheduler_free_dns_callback_ctx(done_ctx);
    omni_uv_getaddrinfo_req_free(done_req);
    return resolved;
}

// ============================================================
// (async-sleep ms) — blocking sleep (milliseconds)
// ============================================================
