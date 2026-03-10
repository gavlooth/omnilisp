module lisp;

import std::core::mem;
import std::io;
import main;

fn Value* tls_raise_async(Interp* interp, char[] code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "io", code, message, null);
}

fn Value* tls_raise_fiber_required(Interp* interp, char[] op_name, char[] code) {
    char[112] ebuf;
    char[] msg = io::bprintf(&ebuf, "%s: requires running fiber context", (ZString)op_name)!!;
    return tls_raise_async(interp, code, msg);
}

fn Value* tls_raise_loop_unavailable(Interp* interp, char[] op_name, char[] code) {
    char[112] ebuf;
    char[] msg = io::bprintf(&ebuf, "%s: libuv loop unavailable", (ZString)op_name)!!;
    return tls_raise_async(interp, code, msg);
}

fn Value* tls_require_async_runtime(Interp* interp, char[] op_name, char[] fiber_code, char[] loop_code) {
    if (!scheduler_is_running_fiber_context()) {
        return tls_raise_fiber_required(interp, op_name, fiber_code);
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return tls_raise_loop_unavailable(interp, op_name, loop_code);
    }
    return null;
}

fn Value* tls_run_fiber_offload(OffloadWork* work, Interp* interp) {
    usz fiber_id = g_scheduler.current_fiber_id;
    Value* start_err = scheduler_begin_fiber_offload(work, fiber_id, interp);
    if (start_err != null) return start_err;

    Value*[1] yargs;
    yargs[0] = make_nil(interp);
    Value* y = prim_yield(yargs[..], null, interp);
    if (y != null && y.tag == ERROR) return y;

    return scheduler_consume_pending_offload(fiber_id, interp);
}

// ============================================================
// (tls-connect tcp-handle hostname [ca-bundle-path] [client-cert-pem-path client-key-pem-path] [resume-session?]) -> tls-handle
// ============================================================

fn bool tls_is_resume_policy_value(Value* v, Interp* interp) {
    return is_symbol(v) && (v.sym_val == interp.sym_true || v.sym_val == interp.sym_false);
}

fn Value* prim_tls_connect(Value*[] args, Env* env, Interp* interp) {
    Value* tcp_v = null;
    Value* host_v = null;
    Value* ca_bundle_v = null;
    Value* client_cert_v = null;
    Value* client_key_v = null;
    Value* resume_policy_v = null;
    Value*[6] argv;
    usz argc = 0;
    char[] expected = "tls-connect: expected (tls-connect tcp-handle hostname [ca-bundle-path] [client-cert-pem-path client-key-pem-path] [resume-session?])";

    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        while (is_cons(packed)) {
            if (argc >= argv.len) return tls_raise_async(interp, "io/tls-connect-expected-args", expected);
            argv[argc++] = car(packed);
            packed = cdr(packed);
        }
        if (!is_nil(packed)) return tls_raise_async(interp, "io/tls-connect-expected-args", expected);
    } else {
        if (args.len < 2 || args.len > argv.len) return tls_raise_async(interp, "io/tls-connect-expected-args", expected);
        for (usz i = 0; i < args.len; i++) argv[i] = args[i];
        argc = args.len;
    }

    if (argc < 2) return tls_raise_async(interp, "io/tls-connect-expected-args", expected);

    tcp_v = argv[0];
    host_v = argv[1];
    usz tail_len = argc - 2;

    switch (tail_len) {
        case 0:
            break;
        case 1:
            if (tls_is_resume_policy_value(argv[2], interp)) {
                resume_policy_v = argv[2];
            } else {
                ca_bundle_v = argv[2];
            }
            break;
        case 2:
            if (tls_is_resume_policy_value(argv[3], interp)) {
                ca_bundle_v = argv[2];
                resume_policy_v = argv[3];
            } else {
                client_cert_v = argv[2];
                client_key_v = argv[3];
            }
            break;
        case 3:
            if (tls_is_resume_policy_value(argv[4], interp)) {
                client_cert_v = argv[2];
                client_key_v = argv[3];
                resume_policy_v = argv[4];
            } else {
                ca_bundle_v = argv[2];
                client_cert_v = argv[3];
                client_key_v = argv[4];
            }
            break;
        case 4:
            ca_bundle_v = argv[2];
            client_cert_v = argv[3];
            client_key_v = argv[4];
            resume_policy_v = argv[5];
            break;
        default:
            return tls_raise_async(interp, "io/tls-connect-expected-args", expected);
    }

    if (!is_string(host_v)) return tls_raise_async(interp, "io/tls-connect-expected-hostname-string", "tls-connect: hostname must be a string");
    if (ca_bundle_v != null && !is_string(ca_bundle_v)) return tls_raise_async(interp, "io/tls-connect-expected-ca-bundle-path-string", "tls-connect: ca-bundle-path must be a string");
    if (client_cert_v != null && !is_string(client_cert_v)) return tls_raise_async(interp, "io/tls-connect-expected-client-cert-path-string", "tls-connect: client-cert-pem-path must be a string");
    if (client_key_v != null && !is_string(client_key_v)) return tls_raise_async(interp, "io/tls-connect-expected-client-key-path-string", "tls-connect: client-key-pem-path must be a string");
    if (resume_policy_v != null && !tls_is_resume_policy_value(resume_policy_v, interp)) {
        return tls_raise_async(interp, "io/tls-connect-expected-resume-policy-bool", "tls-connect: resume-session? must be true or false");
    }
    if ((client_cert_v == null) != (client_key_v == null)) {
        return tls_raise_async(interp, "io/tls-connect-client-cert-key-pair-required", "tls-connect: client cert and key paths must be provided together");
    }

    bool resume_session = resume_policy_v != null && resume_policy_v.sym_val == interp.sym_true;
    TcpHandle* tcp = get_tcp_handle(tcp_v);
    // fault: lisp::TYPE_MISMATCH
    if (tcp == null || !tcp.connected) return tls_raise_async(interp, "io/tls-connect-invalid-tcp-handle", "tls-connect: invalid TCP handle");

    Value* runtime_err = tls_require_async_runtime(
        interp,
        "tls-connect",
        "io/tls-connect-fiber-required",
        "io/tls-connect-libuv-unavailable"
    );
    if (runtime_err != null) return runtime_err;

    TlsConnectOffloadReq* req = (TlsConnectOffloadReq*)mem::malloc(TlsConnectOffloadReq.sizeof);
    if (req == null) return tls_raise_async(interp, "io/tls-connect-out-of-memory", "tls-connect: out of memory");
    *req = {};
    req.fd = tcp.fd;
    req.resume_session = resume_session;
    req.host = tls_dup_string(host_v.str_chars[:host_v.str_len], 255);
    if (req.host == null) {
        tls_connect_req_free(req);
        return tls_raise_async(interp, "io/tls-connect-out-of-memory", "tls-connect: out of memory");
    }
    if (ca_bundle_v != null) {
        req.ca_bundle_path = tls_dup_string(ca_bundle_v.str_chars[:ca_bundle_v.str_len]);
        if (req.ca_bundle_path == null) {
            tls_connect_req_free(req);
            return tls_raise_async(interp, "io/tls-connect-out-of-memory", "tls-connect: out of memory");
        }
    }
    if (client_cert_v != null) {
        req.client_cert_path = tls_dup_string(client_cert_v.str_chars[:client_cert_v.str_len]);
        req.client_key_path = tls_dup_string(client_key_v.str_chars[:client_key_v.str_len]);
        if (req.client_cert_path == null || req.client_key_path == null) {
            tls_connect_req_free(req);
            return tls_raise_async(interp, "io/tls-connect-out-of-memory", "tls-connect: out of memory");
        }
    }

    OffloadWork work = {};
    work.op = OFFLOAD_TLS_CONNECT;
    work.int_arg = (long)(uptr)req;
    usz fiber_id = g_scheduler.current_fiber_id;
    Value* start_err = scheduler_begin_fiber_offload(&work, fiber_id, interp);
    if (start_err != null) {
        tls_connect_req_free(req);
        return start_err;
    }

    Value*[1] yargs;
    yargs[0] = make_nil(interp);
    Value* y = prim_yield(yargs[..], null, interp);
    if (y != null && y.tag == ERROR) return y;

    Value* out = scheduler_consume_pending_offload(fiber_id, interp);
    if (out != null && out.tag == ERROR) return out;
    if (!is_int(out)) return tls_raise_async(interp, "io/tls-connect-invalid-worker-result", "tls-connect: worker returned invalid result");

    TlsHandle* th = (TlsHandle*)(uptr)out.int_val;
    if (th == null) return tls_raise_async(interp, "io/tls-connect-handle-allocation-failed", "tls-connect: failed to allocate TLS handle");

    Value* handle_v = make_tls_handle_val(interp, th);
    if (handle_v != null && handle_v.tag == ERROR) {
        tls_destroy_handle(th);
    }
    return handle_v;
}

// ============================================================
// (tls-server-wrap tcp-handle cert-pem-path key-pem-path) -> tls-handle
// ============================================================

fn Value* prim_tls_server_wrap(Value*[] args, Env* env, Interp* interp) {
    Value* tcp_v = null;
    Value* cert_path_v = null;
    Value* key_path_v = null;

    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        tcp_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) return tls_raise_async(interp, "io/tls-server-wrap-expected-args", "tls-server-wrap: expected (tls-server-wrap tcp-handle cert-pem-path key-pem-path)");
        cert_path_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) return tls_raise_async(interp, "io/tls-server-wrap-expected-args", "tls-server-wrap: expected (tls-server-wrap tcp-handle cert-pem-path key-pem-path)");
        key_path_v = car(packed);
        packed = cdr(packed);
        if (is_cons(packed)) return tls_raise_async(interp, "io/tls-server-wrap-expected-args", "tls-server-wrap: expected (tls-server-wrap tcp-handle cert-pem-path key-pem-path)");
    } else {
        if (args.len < 3) return tls_raise_async(interp, "io/tls-server-wrap-expected-args", "tls-server-wrap: expected (tls-server-wrap tcp-handle cert-pem-path key-pem-path)");
        tcp_v = args[0];
        cert_path_v = args[1];
        key_path_v = args[2];
    }

    if (!is_string(cert_path_v)) return tls_raise_async(interp, "io/tls-server-wrap-expected-cert-path-string", "tls-server-wrap: cert-pem-path must be a string");
    if (!is_string(key_path_v)) return tls_raise_async(interp, "io/tls-server-wrap-expected-key-path-string", "tls-server-wrap: key-pem-path must be a string");

    TcpHandle* tcp = get_tcp_handle(tcp_v);
    if (tcp == null || !tcp.connected || tcp.mode != TCP_STREAM) {
        return tls_raise_async(interp, "io/tls-server-wrap-invalid-tcp-stream-handle", "tls-server-wrap: invalid TCP stream handle");
    }

    TlsHandle* th = (TlsHandle*)mem::malloc(TlsHandle.sizeof);
    if (th == null) return tls_raise_async(interp, "io/tls-server-wrap-out-of-memory", "tls-server-wrap: out of memory");
    *th = {};
    th.in_flight.store(0);

    usz server_ctx_size = omni_tls_server_context_size();
    if (server_ctx_size == 0) {
        mem::free(th);
        return tls_raise_async(interp, "io/tls-server-wrap-invalid-server-context-size", "tls-server-wrap: invalid server context size");
    }

    th.client_ctx = mem::malloc(server_ctx_size);
    th.sslio_ctx = mem::malloc(BEARSSL_SSLIO_CTX_SIZE);
    th.iobuf = mem::malloc(BEARSSL_IOBUF_SIZE);
    th.fd_ptr = (int*)mem::malloc(int.sizeof);
    if (th.client_ctx == null || th.sslio_ctx == null || th.iobuf == null || th.fd_ptr == null) {
        tls_handle_free(th);
        mem::free(th);
        return tls_raise_async(interp, "io/tls-server-wrap-out-of-memory", "tls-server-wrap: out of memory");
    }

    th.server_creds = omni_tls_server_creds_load((ZString)cert_path_v.str_chars, (ZString)key_path_v.str_chars);
    if (th.server_creds == null) {
        tls_handle_free(th);
        mem::free(th);
        return tls_raise_async(interp, "io/tls-server-wrap-credentials-load-failed", "tls-server-wrap: failed to load server cert/key PEM files (RSA key required)");
    }

    void* chain = omni_tls_server_creds_chain(th.server_creds);
    usz chain_len = omni_tls_server_creds_chain_len(th.server_creds);
    void* rsa_key = omni_tls_server_creds_rsa_key(th.server_creds);
    if (chain == null || chain_len == 0 || rsa_key == null) {
        tls_handle_free(th);
        mem::free(th);
        return tls_raise_async(interp, "io/tls-server-wrap-invalid-server-credentials", "tls-server-wrap: invalid server credentials");
    }

    *th.fd_ptr = tcp.fd;
    th.connected = true;

    br_ssl_server_init_full_rsa(th.client_ctx, chain, chain_len, rsa_key);
    br_ssl_engine_set_buffer(th.client_ctx, th.iobuf, BEARSSL_IOBUF_SIZE, 1);
    if (br_ssl_server_reset(th.client_ctx) == 0) {
        tls_handle_free(th);
        mem::free(th);
        return tls_raise_async(interp, "io/tls-server-wrap-server-reset-failed", "tls-server-wrap: TLS server reset failed");
    }

    br_sslio_init(th.sslio_ctx, th.client_ctx,
        (BrSslioCallback)&omni_tls_sock_read, th.fd_ptr,
        (BrSslioCallback)&omni_tls_sock_write, th.fd_ptr);

    return make_tls_handle_val(interp, th);
}

// ============================================================
// (tls-read tls-handle [max-bytes]) -> string
// ============================================================

fn Value* prim_tls_read(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return tls_raise_async(interp, "io/tls-read-expected-handle-max-bytes", "tls-read: expected (tls-read handle [max-bytes])");

    Value* runtime_err = tls_require_async_runtime(
        interp,
        "tls-read",
        "io/tls-read-fiber-required",
        "io/tls-read-libuv-unavailable"
    );
    if (runtime_err != null) return runtime_err;

    TlsHandle* th = get_tls_handle(args[0]);
    if (th == null) return tls_raise_async(interp, "io/tls-read-invalid-or-closed-handle", "tls-read: invalid or closed handle");
    if (!tls_try_mark_in_flight(th)) return tls_raise_handle_busy(interp, "tls-read");
    defer tls_unmark_in_flight(th);
    if (!th.connected) return tls_raise_async(interp, "io/tls-read-invalid-or-closed-handle", "tls-read: invalid or closed handle");

    usz max_bytes = 4096;
    if (args.len >= 2 && is_int(args[1])) {
        max_bytes = (usz)args[1].int_val;
        if (max_bytes > 65536) max_bytes = 65536;
    }

    OffloadWork work = {};
    work.op = OFFLOAD_TLS_READ;
    work.int_arg = (long)(uptr)th;
    work.int_arg2 = (long)max_bytes;
    Value* out = tls_run_fiber_offload(&work, interp);
    if (out != null && out.tag == ERROR) return out;
    if (out == null) return make_string(interp, "");
    return out;
}

// ============================================================
// (tls-write tls-handle data) -> bytes written
// ============================================================

fn Value* prim_tls_write(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 2) return tls_raise_async(interp, "io/tls-write-expected-handle-data", "tls-write: expected (tls-write handle data)");
    // fault: lisp::EXPECTED_STRING
    if (!is_string(args[1])) return tls_raise_async(interp, "io/tls-write-expected-data-string", "tls-write: data must be a string");

    Value* runtime_err = tls_require_async_runtime(
        interp,
        "tls-write",
        "io/tls-write-fiber-required",
        "io/tls-write-libuv-unavailable"
    );
    if (runtime_err != null) return runtime_err;

    TlsHandle* th = get_tls_handle(args[0]);
    if (th == null) return tls_raise_async(interp, "io/tls-write-invalid-or-closed-handle", "tls-write: invalid or closed handle");
    if (!tls_try_mark_in_flight(th)) return tls_raise_handle_busy(interp, "tls-write");
    defer tls_unmark_in_flight(th);
    if (!th.connected) return tls_raise_async(interp, "io/tls-write-invalid-or-closed-handle", "tls-write: invalid or closed handle");

    char[] data = args[1].str_chars[:args[1].str_len];
    OffloadWork work = {};
    work.op = OFFLOAD_TLS_WRITE;
    work.int_arg = (long)(uptr)th;
    work.shared = scheduler_domain_publish_shared(scheduler_current_domain(), data);
    if (work.shared == null) return tls_raise_async(interp, "io/tls-write-out-of-memory", "tls-write: out of memory");

    Value* out = tls_run_fiber_offload(&work, interp);
    if (out != null && out.tag == ERROR) return out;
    if (is_int(out)) return out;
    return make_int(interp, 0);
}

// ============================================================
// (tls-close tls-handle) -> nil
// ============================================================

fn Value* prim_tls_close(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return tls_raise_async(interp, "io/tls-close-expected-handle", "tls-close: expected (tls-close handle)");

    Value* runtime_err = tls_require_async_runtime(
        interp,
        "tls-close",
        "io/tls-close-fiber-required",
        "io/tls-close-libuv-unavailable"
    );
    if (runtime_err != null) return runtime_err;

    TlsHandle* th = get_tls_handle(args[0]);
    if (th == null) return tls_raise_async(interp, "io/tls-close-invalid-handle", "tls-close: invalid handle");
    if (!tls_try_mark_in_flight(th)) return tls_raise_handle_busy(interp, "tls-close");
    defer tls_unmark_in_flight(th);

    OffloadWork work = {};
    work.op = OFFLOAD_TLS_CLOSE;
    work.int_arg = (long)(uptr)th;
    Value* out = tls_run_fiber_offload(&work, interp);
    if (out != null && out.tag == ERROR) return out;
    return make_nil(interp);
}
