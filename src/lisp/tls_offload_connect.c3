module lisp;

import std::core::mem;
import std::io;

struct TlsConnectOffloadReq {
    int fd;
    bool resume_session;
    char* host;
    char* ca_bundle_path;
    char* client_cert_path;
    char* client_key_path;
}

fn char* tls_dup_string(char[] src, usz max_len = usz.max) {
    usz len = src.len;
    if (len > max_len) return null;
    if (len == usz.max) return null;
    char* out = (char*)mem::malloc(len + 1);
    if (out == null) return null;
    for (usz i = 0; i < len; i++) out[i] = src[i];
    out[len] = 0;
    return out;
}

fn void tls_connect_req_free(TlsConnectOffloadReq* req) {
    if (req == null) return;
    if (req.host != null) mem::free(req.host);
    if (req.ca_bundle_path != null) mem::free(req.ca_bundle_path);
    if (req.client_cert_path != null) mem::free(req.client_cert_path);
    if (req.client_key_path != null) mem::free(req.client_key_path);
    mem::free(req);
}

fn char[] tls_offload_error_with_cause(char[256]* buf, char[] base, TlsHandle* th = null) {
    char[] cause = tls_last_error_text();
    if (cause.len == 0 && th != null) {
        cause = tls_engine_error_text(th.client_ctx);
    }
    if (cause.len == 0) return base;
    if (try char[] msg = io::bprintf(buf, "%s: %s", (ZString)base, (ZString)cause)) {
        return msg;
    }
    return base;
}

fn bool scheduler_offload_tls_connect(long req_raw, OffloadCompletion* completion) {
    TlsConnectOffloadReq* req = (TlsConnectOffloadReq*)(uptr)req_raw;
    if (req == null || req.host == null || req.fd < 0) {
        scheduler_set_offload_error(completion, "tls-connect: invalid request");
        return false;
    }
    defer tls_connect_req_free(req);

    TlsHandle* th = (TlsHandle*)mem::malloc(TlsHandle.sizeof);
    if (th == null) {
        scheduler_set_offload_error(completion, "tls-connect: out of memory");
        return false;
    }
    *th = {};
    th.in_flight.store(0);

    th.client_ctx = mem::malloc(BEARSSL_CLIENT_CTX_SIZE);
    th.x509_ctx = mem::malloc(BEARSSL_X509_CTX_SIZE);
    th.sslio_ctx = mem::malloc(BEARSSL_SSLIO_CTX_SIZE);
    th.iobuf = mem::malloc(BEARSSL_IOBUF_SIZE);
    th.fd_ptr = (int*)mem::malloc(int.sizeof);

    if (th.client_ctx == null || th.x509_ctx == null || th.sslio_ctx == null ||
        th.iobuf == null || th.fd_ptr == null) {
        tls_destroy_handle(th);
        scheduler_set_offload_error(completion, "tls-connect: out of memory");
        return false;
    }

    *th.fd_ptr = req.fd;
    th.connected = true;
    th.resume_session = req.resume_session;

    if (th.resume_session) {
        usz host_len = 0;
        while (req.host[host_len] != 0) host_len++;
        th.session_host = (char*)mem::malloc(host_len + 1);
        if (th.session_host == null) {
            tls_destroy_handle(th);
            scheduler_set_offload_error(completion, "tls-connect: out of memory");
            return false;
        }
        for (usz i = 0; i < host_len; i++) th.session_host[i] = req.host[i];
        th.session_host[host_len] = 0;
    }

    if (req.ca_bundle_path != null) {
        omni_tls_clear_last_error();
        th.trust_store = omni_tls_trust_store_load_file((ZString)req.ca_bundle_path);
    } else {
        omni_tls_clear_last_error();
        th.trust_store = omni_tls_trust_store_load_default();
    }
    if (th.trust_store == null) {
        char[256] ebuf;
        char[] msg = req.ca_bundle_path != null
            ? tls_offload_error_with_cause(&ebuf, "tls-connect: failed to load CA trust store from ca-bundle-path")
            : tls_offload_error_with_cause(&ebuf, "tls-connect: failed to load CA trust store (set OMNI_TLS_CA_FILE or SSL_CERT_FILE)");
        tls_destroy_handle(th);
        scheduler_set_offload_error(completion, msg);
        return false;
    }

    void* trust_anchors = omni_tls_trust_store_get_anchors(th.trust_store);
    usz trust_anchor_count = omni_tls_trust_store_get_anchor_count(th.trust_store);
    if (trust_anchors == null || trust_anchor_count == 0) {
        tls_destroy_handle(th);
        scheduler_set_offload_error(completion, "tls-connect: trust store has no usable CA certificates");
        return false;
    }

    br_ssl_client_init_full(th.client_ctx, th.x509_ctx, trust_anchors, trust_anchor_count);

    if (req.client_cert_path != null) {
        omni_tls_clear_last_error();
        th.server_creds = omni_tls_server_creds_load((ZString)req.client_cert_path, (ZString)req.client_key_path);
        int client_cert_status = th.server_creds != null ? omni_tls_client_set_single_rsa(th.client_ctx, th.server_creds) : 0;
        if (th.server_creds == null || client_cert_status <= 0) {
            char[256] ebuf;
            char[] msg = tls_offload_error_with_cause(&ebuf, "tls-connect: failed to configure client cert/key");
            tls_destroy_handle(th);
            scheduler_set_offload_error(completion, msg);
            return false;
        }
    }

    br_ssl_engine_set_buffer(th.client_ctx, th.iobuf, BEARSSL_IOBUF_SIZE, 1);
    if (th.resume_session && th.session_host != null) {
        omni_tls_client_session_cache_load(th.client_ctx, (ZString)th.session_host);
    }

    int resume_flag = th.resume_session ? 1 : 0;
    omni_tls_clear_last_error();
    int reset_ok = br_ssl_client_reset(th.client_ctx, req.host, resume_flag);
    if (reset_ok == 0) {
        char[256] ebuf;
        char[] msg = tls_offload_error_with_cause(&ebuf, "tls-connect: TLS client reset failed", th);
        tls_destroy_handle(th);
        scheduler_set_offload_error(completion, msg);
        return false;
    }

    br_sslio_init(th.sslio_ctx, th.client_ctx,
        (BrSslioCallback)&omni_tls_sock_read, th.fd_ptr,
        (BrSslioCallback)&omni_tls_sock_write, th.fd_ptr);

    completion.kind = OFFLOAD_RES_INT;
    completion.int_result = scheduler_domain_publish_sendable_int(
        scheduler_primary_domain(),
        (long)(uptr)th
    );
    return true;
}
