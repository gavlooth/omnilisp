module lisp;

fn bool scheduler_offload_compress(bool gzip_mode, SharedHandle* input, OffloadCompletion* completion) {
    char[] input_bytes = "";
    if (!scheduler_project_shared(input, &input_bytes)) {
        scheduler_set_offload_error(completion, "offload: missing input");
        return false;
    }
    usz input_len = input_bytes.len;

    DeflateCompressor* compressor = libdeflate_alloc_compressor(6);
    if (compressor == null) {
        scheduler_set_offload_error(completion, "offload: compressor allocation failed");
        return false;
    }
    defer libdeflate_free_compressor(compressor);

    usz bound = gzip_mode
        ? libdeflate_gzip_compress_bound(compressor, input_len)
        : libdeflate_deflate_compress_bound(compressor, input_len);
    if (bound == usz.max) {
        scheduler_set_offload_error(completion, "offload: out of memory");
        return false;
    }
    char* out = (char*)mem::malloc(bound + 1);
    if (out == null) {
        scheduler_set_offload_error(completion, "offload: out of memory");
        return false;
    }

    usz written = gzip_mode
        ? libdeflate_gzip_compress(compressor, input_bytes, input_len, out, bound)
        : libdeflate_deflate_compress(compressor, input_bytes, input_len, out, bound);
    if (written == 0) {
        mem::free(out);
        scheduler_set_offload_error(completion, "offload: compression failed");
        return false;
    }
    out[written] = 0;

    return scheduler_set_offload_bytes_owned(
        completion,
        out,
        written,
        "offload: result allocation failed"
    );
}

fn bool scheduler_offload_accept_fd(long raw_fd, OffloadCompletion* completion) {
    if (raw_fd < 0) {
        scheduler_set_offload_error(completion, "offload accept-fd: invalid listener fd");
        return false;
    }
    if (raw_fd > (long)int.max) {
        scheduler_set_offload_error(completion, "offload accept-fd: invalid listener fd");
        return false;
    }
    int accepted_fd = tcp_accept_fd((int)raw_fd);
    if (accepted_fd < 0) {
        scheduler_set_offload_error(completion, "offload accept-fd: accept failed");
        return false;
    }
    completion.kind = OFFLOAD_RES_INT;
    completion.int_result = scheduler_domain_publish_sendable_int(
        scheduler_primary_domain(),
        accepted_fd
    );
    return true;
}

fn bool scheduler_offload_tcp_connect(SharedHandle* host_blob, int port, OffloadCompletion* completion) {
    char[] host = "";
    if (!scheduler_project_shared(host_blob, &host)) {
        scheduler_set_offload_error(completion, "offload tcp-connect: missing host");
        return false;
    }
    // host is stored as null-terminated bytes in the blob
    void* result = null;
    if (!resolve_host_and_port(host, port, &result)) {
        scheduler_set_offload_error(completion, "offload tcp-connect: DNS resolution failed");
        return false;
    }
    int fd = connect_fd_from_addrinfo(result);
    c_freeaddrinfo(result);
    if (fd < 0) {
        scheduler_set_offload_error(completion, "offload tcp-connect: connection failed");
        return false;
    }
    completion.kind = OFFLOAD_RES_INT;
    completion.int_result = scheduler_domain_publish_sendable_int(
        scheduler_primary_domain(),
        fd
    );
    return true;
}

fn bool scheduler_offload_http_get(SharedHandle* url_blob, OffloadCompletion* completion) {
    char[] url = "";
    if (!scheduler_project_shared(url_blob, &url)) {
        scheduler_set_offload_error(completion, "offload http-get: missing url");
        return false;
    }
    ParsedUrl parsed = {};
    if (catch err = parse_url(url, &parsed)) {
        scheduler_set_offload_error(completion, "offload http-get: invalid url");
        return false;
    }

    // DNS + TCP connect
    void* ai_result = null;
    if (!resolve_host_and_port(parsed.host[:parsed.host_len], parsed.port, &ai_result)) {
        scheduler_set_offload_error(completion, "offload http-get: DNS resolution failed");
        return false;
    }
    int fd = connect_fd_from_addrinfo(ai_result);
    c_freeaddrinfo(ai_result);
    if (fd < 0) {
        scheduler_set_offload_error(completion, "offload http-get: connection failed");
        return false;
    }

    // Build HTTP GET request
    char[4096] req_buf;
    usz? req_len_opt = build_request("GET", &parsed,
        "Accept: application/json\r\nUser-Agent: finwatch/1.0",
        "", &req_buf, 4096);
    if (catch err = req_len_opt) {
        c_close_fd(fd);
        scheduler_set_offload_error(completion, "offload http-get: request too large");
        return false;
    }
    usz req_len = (usz)req_len_opt;

    bool ok = false;

    if (parsed.is_https) {
        void* client_ctx = mem::malloc(BEARSSL_CLIENT_CTX_SIZE);
        void* x509_ctx   = mem::malloc(BEARSSL_X509_CTX_SIZE);
        void* sslio_ctx  = mem::malloc(BEARSSL_SSLIO_CTX_SIZE);
        void* iobuf      = mem::malloc(BEARSSL_IOBUF_SIZE);
        int*  fd_ptr     = (int*)mem::malloc(int.sizeof);
        void* trust_store = omni_tls_trust_store_load_default();
        bool sslio_inited = false;

        if (client_ctx != null && x509_ctx != null && sslio_ctx != null &&
                iobuf != null && fd_ptr != null && trust_store != null) {
            *fd_ptr = fd;
            void* trust_anchors = omni_tls_trust_store_get_anchors(trust_store);
            usz trust_anchor_count = omni_tls_trust_store_get_anchor_count(trust_store);
            if (trust_anchors != null && trust_anchor_count > 0) {
                br_ssl_client_init_full(client_ctx, x509_ctx, trust_anchors, trust_anchor_count);
                br_ssl_engine_set_buffer(client_ctx, iobuf, BEARSSL_IOBUF_SIZE, 1);
                int reset_ok = br_ssl_client_reset(client_ctx, &parsed.host, 0);
                if (reset_ok != 0) {
                    br_sslio_init(sslio_ctx, client_ctx,
                        (BrSslioCallback)&omni_tls_sock_read, fd_ptr,
                        (BrSslioCallback)&omni_tls_sock_write, fd_ptr);
                    sslio_inited = true;

                    if (br_sslio_write_all(sslio_ctx, &req_buf, req_len) >= 0 &&
                            br_sslio_flush(sslio_ctx) >= 0) {
                        char* resp_buf = null;
                        usz resp_len = 0;
                        usz resp_cap = 0;
                        bool read_ok = true;
                        char[4096] chunk;
                        while (true) {
                            int n = br_sslio_read(sslio_ctx, &chunk, chunk.len);
                            if (n < 0) {
                                scheduler_discard_offload_response_buffer(&resp_buf, &resp_len, &resp_cap);
                                scheduler_set_offload_error(completion, "offload http-get: TLS read failed");
                                read_ok = false;
                                break;
                            }
                            if (n == 0) break;
                            if (!scheduler_append_offload_response_chunk(
                                    &resp_buf,
                                    &resp_len,
                                    &resp_cap,
                                    &chunk,
                                    (usz)n,
                                    completion
                                )) {
                                read_ok = false;
                                break;
                            }
                        }
                        if (read_ok && scheduler_publish_offload_response_owned(completion, resp_buf, resp_len)) {
                            ok = true;
                        }
                    }
                }
            }
            if (sslio_inited) br_sslio_close(sslio_ctx);
        }

        if (client_ctx != null) mem::free(client_ctx);
        if (x509_ctx   != null) mem::free(x509_ctx);
        if (sslio_ctx  != null) mem::free(sslio_ctx);
        if (iobuf      != null) mem::free(iobuf);
        if (fd_ptr     != null) mem::free(fd_ptr);
        if (trust_store != null) omni_tls_trust_store_free(trust_store);
    } else {
        if (scheduler_send_all_fd(fd, &req_buf, req_len)) {
            char* resp_buf = null;
            usz resp_len = 0;
            usz resp_cap = 0;
            bool read_ok = true;
            char[4096] chunk;
            while (true) {
                long n = c_recv(fd, &chunk, chunk.len, 0);
                if (n < 0) {
                    scheduler_discard_offload_response_buffer(&resp_buf, &resp_len, &resp_cap);
                    scheduler_set_offload_error(completion, "offload http-get: read failed");
                    read_ok = false;
                    break;
                }
                if (n == 0) break;
                if (!scheduler_append_offload_response_chunk(
                        &resp_buf,
                        &resp_len,
                        &resp_cap,
                        &chunk,
                        (usz)n,
                        completion
                    )) {
                    read_ok = false;
                    break;
                }
            }
            if (read_ok && scheduler_publish_offload_response_owned(completion, resp_buf, resp_len)) {
                ok = true;
            }
        }
    }

    c_close_fd(fd);
    if (!ok && completion.kind != OFFLOAD_RES_ERROR) {
        scheduler_set_offload_error(completion, "offload http-get: request failed");
    }
    return ok;
}
