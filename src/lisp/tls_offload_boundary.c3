module lisp;

import std::core::mem;

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
    if (len > max_len) len = max_len;
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
        th.trust_store = omni_tls_trust_store_load_file((ZString)req.ca_bundle_path);
    } else {
        th.trust_store = omni_tls_trust_store_load_default();
    }
    if (th.trust_store == null) {
        tls_destroy_handle(th);
        if (req.ca_bundle_path != null) {
            scheduler_set_offload_error(completion, "tls-connect: failed to load CA trust store from ca-bundle-path");
        } else {
            scheduler_set_offload_error(completion, "tls-connect: failed to load CA trust store (set OMNI_TLS_CA_FILE or SSL_CERT_FILE)");
        }
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
        th.server_creds = omni_tls_server_creds_load((ZString)req.client_cert_path, (ZString)req.client_key_path);
        int client_cert_status = th.server_creds != null ? omni_tls_client_set_single_rsa(th.client_ctx, th.server_creds) : 0;
        if (th.server_creds == null || client_cert_status <= 0) {
            tls_destroy_handle(th);
            scheduler_set_offload_error(completion, "tls-connect: failed to configure client cert/key (PEM RSA required)");
            return false;
        }
    }

    br_ssl_engine_set_buffer(th.client_ctx, th.iobuf, BEARSSL_IOBUF_SIZE, 1);
    if (th.resume_session && th.session_host != null) {
        omni_tls_client_session_cache_load(th.client_ctx, (ZString)th.session_host);
    }

    int resume_flag = th.resume_session ? 1 : 0;
    int reset_ok = br_ssl_client_reset(th.client_ctx, req.host, resume_flag);
    if (reset_ok == 0) {
        tls_destroy_handle(th);
        scheduler_set_offload_error(completion, "tls-connect: TLS client reset failed");
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

fn bool scheduler_offload_tls_read(long tls_raw, long max_bytes_raw, OffloadCompletion* completion) {
    TlsHandle* th = (TlsHandle*)(uptr)tls_raw;
    if (th == null || !th.connected || th.sslio_ctx == null) {
        scheduler_set_offload_error(completion, "tls-read: invalid or closed handle");
        return false;
    }

    usz max_bytes = 4096;
    if (max_bytes_raw > 0) {
        max_bytes = (usz)max_bytes_raw;
    }
    if (max_bytes > 65536) max_bytes = 65536;
    if (max_bytes == 0) max_bytes = 1;

    char* tmp = (char*)mem::malloc(max_bytes);
    if (tmp == null) {
        scheduler_set_offload_error(completion, "tls-read: out of memory");
        return false;
    }
    defer mem::free(tmp);

    int received = 0;
    for (int attempt = 0; attempt < TLS_READ_EMPTY_RETRY_MAX; attempt++) {
        received = br_sslio_read(th.sslio_ctx, tmp, max_bytes);
        if (received < 0) {
            scheduler_set_offload_error(completion, "tls-read: read failed");
            return false;
        }
        if (received > 0) break;
        if (attempt + 1 < TLS_READ_EMPTY_RETRY_MAX) {
            c_usleep(TLS_READ_EMPTY_RETRY_US);
        }
    }

    usz out_len = received > 0 ? (usz)received : 0;
    char* out = (char*)mem::malloc(out_len + 1);
    if (out == null) {
        scheduler_set_offload_error(completion, "tls-read: out of memory");
        return false;
    }
    for (usz i = 0; i < out_len; i++) out[i] = tmp[i];
    out[out_len] = 0;

    return scheduler_set_offload_bytes_owned(
        completion,
        out,
        out_len,
        "tls-read: result allocation failed"
    );
}

fn bool scheduler_offload_tls_write(long tls_raw, SharedHandle* data_blob, OffloadCompletion* completion) {
    TlsHandle* th = (TlsHandle*)(uptr)tls_raw;
    if (th == null || !th.connected || th.sslio_ctx == null) {
        scheduler_set_offload_error(completion, "tls-write: invalid or closed handle");
        return false;
    }

    char[] data = "";
    if (!scheduler_project_shared(data_blob, &data)) {
        scheduler_set_offload_error(completion, "tls-write: missing payload");
        return false;
    }

    int status = br_sslio_write_all(th.sslio_ctx, data.ptr, data.len);
    if (status < 0) {
        scheduler_set_offload_error(completion, "tls-write: write failed");
        return false;
    }

    int flush_status = br_sslio_flush(th.sslio_ctx);
    if (flush_status < 0) {
        scheduler_set_offload_error(completion, "tls-write: flush failed");
        return false;
    }

    completion.kind = OFFLOAD_RES_INT;
    completion.int_result = scheduler_domain_publish_sendable_int(
        scheduler_primary_domain(),
        (long)data.len
    );
    return true;
}

fn bool scheduler_offload_tls_close(long tls_raw, OffloadCompletion* completion) {
    TlsHandle* th = (TlsHandle*)(uptr)tls_raw;
    if (th == null) {
        scheduler_set_offload_error(completion, "tls-close: invalid handle");
        return false;
    }

    if (th.connected && th.sslio_ctx != null) {
        tls_maybe_store_client_session(th);
        br_sslio_close(th.sslio_ctx);
        tls_handle_free(th);
        th.connected = false;
    }

    completion.kind = OFFLOAD_RES_NIL;
    return true;
}
