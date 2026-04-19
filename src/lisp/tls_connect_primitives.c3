module lisp;

import std::core::mem;
import main;

// ============================================================
// (tls-connect tcp-handle hostname [ca-bundle-path] [client-cert-pem-path client-key-pem-path] [resume-session?]) -> tls-handle
// ============================================================

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
    if (y != null && y.tag == ERROR) {
        scheduler_close_pending_offload(fiber_id);
        return y;
    }

    Value* out = scheduler_consume_pending_offload(fiber_id, interp);
    if (out != null && out.tag == ERROR) return out;
    if (!is_int(out)) return tls_raise_async(interp, "io/tls-connect-invalid-worker-result", "tls-connect: worker returned invalid result");

    TlsHandle* th = (TlsHandle*)(uptr)out.int_val;
    if (th == null) return tls_raise_async(interp, "io/tls-connect-handle-allocation-failed", "tls-connect: failed to allocate TLS handle");

    Value* handle_v = make_tls_handle_val(interp, th, "io/tls-connect-out-of-memory", "tls-connect: out of memory");
    return handle_v;
}

// Regression helper for trust-store cause preservation.
// Not exported into the public Omni surface.
fn Value* prim_tls_trust_store_load_cause(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return tls_raise_async(interp, "io/tls-trust-store-load-cause-expected-path", "tls-trust-store-load-cause: expected (tls-trust-store-load-cause path)");
    if (!is_string(args[0])) return tls_raise_async(interp, "io/tls-trust-store-load-cause-path-not-string", "tls-trust-store-load-cause: path must be a string");

    omni_tls_clear_last_error();
    void* store = omni_tls_trust_store_load_file((ZString)args[0].str_chars);
    char[] msg = tls_last_error_text();
    if (store != null) omni_tls_trust_store_free(store);
    return make_string(interp, msg);
}
