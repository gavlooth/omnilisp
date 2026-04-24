module lisp;

import std::core::mem;
import main;

// ============================================================
// (tls-server-wrap tcp-handle cert-pem-path key-pem-path) -> tls-handle
// ============================================================

<*
@require interp != null
*>
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
        if (!is_nil(packed)) return tls_raise_async(interp, "io/tls-server-wrap-expected-args", "tls-server-wrap: expected (tls-server-wrap tcp-handle cert-pem-path key-pem-path)");
    } else {
        if (args.len != 3) return tls_raise_async(interp, "io/tls-server-wrap-expected-args", "tls-server-wrap: expected (tls-server-wrap tcp-handle cert-pem-path key-pem-path)");
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

    CApiStringResult cert_path = c_api_string_dup_no_nul(cert_path_v.str_chars[:cert_path_v.str_len]);
    if (cert_path.contains_nul) {
        tls_handle_free(th);
        mem::free(th);
        return tls_raise_async(interp, "io/tls-server-wrap-invalid-cert-path", "tls-server-wrap: cert-pem-path contains embedded NUL byte");
    }
    if (!cert_path.ok) {
        tls_handle_free(th);
        mem::free(th);
        return tls_raise_async(interp, "io/tls-server-wrap-out-of-memory", "tls-server-wrap: out of memory");
    }
    defer c_api_string_free(&cert_path);

    CApiStringResult key_path = c_api_string_dup_no_nul(key_path_v.str_chars[:key_path_v.str_len]);
    if (key_path.contains_nul) {
        tls_handle_free(th);
        mem::free(th);
        return tls_raise_async(interp, "io/tls-server-wrap-invalid-key-path", "tls-server-wrap: key-pem-path contains embedded NUL byte");
    }
    if (!key_path.ok) {
        tls_handle_free(th);
        mem::free(th);
        return tls_raise_async(interp, "io/tls-server-wrap-out-of-memory", "tls-server-wrap: out of memory");
    }
    defer c_api_string_free(&key_path);

    th.server_creds = omni_tls_server_creds_load(cert_path.ptr, key_path.ptr);
    if (th.server_creds == null) {
        char[256] ebuf;
        char[] msg = tls_message_with_cause(&ebuf, "tls-server-wrap: failed to load server cert/key PEM files (RSA key required)");
        tls_handle_free(th);
        mem::free(th);
        return tls_raise_async(interp, "io/tls-server-wrap-credentials-load-failed", msg);
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
    omni_tls_clear_last_error();
    if (br_ssl_server_reset(th.client_ctx) == 0) {
        tls_handle_free(th);
        mem::free(th);
        return tls_raise_async(interp, "io/tls-server-wrap-server-reset-failed", "tls-server-wrap: TLS server reset failed");
    }

    br_sslio_init(th.sslio_ctx, th.client_ctx,
        (BrSslioCallback)&omni_tls_sock_read, th.fd_ptr,
        (BrSslioCallback)&omni_tls_sock_write, th.fd_ptr);

    Value* handle_v = make_tls_handle_val(interp, th, "io/tls-server-wrap-out-of-memory", "tls-server-wrap: out of memory");
    return handle_v;
}
