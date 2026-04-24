module lisp;

import std::core::mem;

fn bool scheduler_offload_tls_read(long tls_raw, long max_bytes_raw, OffloadCompletion* completion) {
    TlsHandle* th = (TlsHandle*)(uptr)tls_raw;
    if (th == null || !th.connected || th.sslio_ctx == null) {
        scheduler_set_offload_error(completion, "tls-read: invalid or closed handle");
        return false;
    }

    if (max_bytes_raw <= 0) {
        scheduler_set_offload_error(completion, "tls-read: max-bytes must be > 0");
        return false;
    }
    usz max_bytes = (usz)max_bytes_raw;
    if (max_bytes > 65536) max_bytes = 65536;

    char* tmp = (char*)mem::malloc(max_bytes);
    if (tmp == null) {
        scheduler_set_offload_error(completion, "tls-read: out of memory");
        return false;
    }
    defer mem::free(tmp);

    int received = 0;
    for (int attempt = 0; attempt < TLS_READ_EMPTY_RETRY_MAX; attempt++) {
        omni_tls_clear_last_error();
        received = br_sslio_read(th.sslio_ctx, tmp, max_bytes);
        if (received < 0) {
            char[256] ebuf;
            scheduler_set_offload_error(completion,
                tls_offload_error_with_cause(&ebuf, "tls-read: read failed", th));
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

    char* data_heap = null;
    usz data_len = 0;
    if (!scheduler_copy_shared_bytes_owned(data_blob, &data_heap, &data_len)) {
        scheduler_set_offload_error(completion, "tls-write: missing payload");
        return false;
    }
    defer mem::free(data_heap);
    char[] data = data_heap[:data_len];

    omni_tls_clear_last_error();
    int status = br_sslio_write_all(th.sslio_ctx, data.ptr, data.len);
    if (status < 0) {
        char[256] ebuf;
        scheduler_set_offload_error(completion,
            tls_offload_error_with_cause(&ebuf, "tls-write: write failed", th));
        return false;
    }

    omni_tls_clear_last_error();
    int flush_status = br_sslio_flush(th.sslio_ctx);
    if (flush_status < 0) {
        char[256] ebuf;
        scheduler_set_offload_error(completion,
            tls_offload_error_with_cause(&ebuf, "tls-write: flush failed", th));
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
