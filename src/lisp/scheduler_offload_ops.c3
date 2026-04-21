module lisp;

import std::io;

fn bool scheduler_offload_read_file(SharedHandle* path_blob, OffloadCompletion* completion) {
    char[] path = "";
    if (!scheduler_project_shared(path_blob, &path)) {
        scheduler_set_offload_error(completion, "offload read-file: missing/invalid path");
        return false;
    }

    char* content_buf = null;
    usz content_len = 0;
    if (!io_uv_read_all_file(path, &content_buf, &content_len)) {
        completion.kind = OFFLOAD_RES_NIL;
        return true;
    }

    return scheduler_set_offload_bytes_owned(
        completion,
        content_buf,
        content_len,
        "offload read-file: result allocation failed"
    );
}

fn bool scheduler_split_write_file_payload(char[] payload, char[]* out_path, char[]* out_content) {
    if (out_path != null) *out_path = "";
    if (out_content != null) *out_content = "";
    if (payload.len == 0) return false;

    usz sep = payload.len;
    for (usz i = 0; i < payload.len; i++) {
        if (payload[i] == 0) {
            sep = i;
            break;
        }
    }
    if (sep == payload.len) return false;

    if (out_path != null) *out_path = payload[:sep];
    if (out_content != null) {
        if (sep + 1 < payload.len) {
            *out_content = payload[sep + 1..];
        } else {
            *out_content = "";
        }
    }
    return true;
}

fn void scheduler_set_offload_error_with_status(OffloadCompletion* completion, char[] message, int status) {
    char[192] ebuf;
    if (try char[] msg = io::bprintf(&ebuf, "%s (%d)", (ZString)message, status)) {
        scheduler_set_offload_error(completion, msg);
        return;
    } else {
        scheduler_set_offload_error(completion, message);
        return;
    }
}

fn bool scheduler_build_atomic_temp_path(
    char[] path,
    usz unique_id,
    char[]* out_temp_path,
    char** out_heap,
    OffloadCompletion* completion
) {
    if (out_temp_path != null) *out_temp_path = "";
    if (out_heap != null) *out_heap = null;

    if (path.len == 0) {
        scheduler_set_offload_error(completion, "offload write-file: invalid path");
        return false;
    }

    char[64] suffix_buf;
    char[] suffix = "";
    if (unique_id > (usz)long.max) {
        scheduler_set_offload_error(completion, "offload write-file: temp path id overflow");
        return false;
    }
    if (try char[] formatted = io::bprintf(&suffix_buf, ".tmp.%d.%x", c_getpid(), unique_id)) {
        suffix = formatted;
    } else {
        scheduler_set_offload_error(completion, "offload write-file: temp path formatting failed");
        return false;
    }

    if (path.len > usz.max - suffix.len) {
        scheduler_set_offload_error(completion, "offload write-file: out of memory");
        return false;
    }
    usz temp_len = path.len + suffix.len;
    if (temp_len == usz.max) {
        scheduler_set_offload_error(completion, "offload write-file: out of memory");
        return false;
    }
    char* temp_heap = (char*)mem::malloc(temp_len + 1);
    if (temp_heap == null) {
        scheduler_set_offload_error(completion, "offload write-file: out of memory");
        return false;
    }
    for (usz i = 0; i < path.len; i++) temp_heap[i] = path[i];
    for (usz i = 0; i < suffix.len; i++) temp_heap[path.len + i] = suffix[i];
    temp_heap[temp_len] = 0;

    if (out_temp_path != null) *out_temp_path = temp_heap[:temp_len];
    if (out_heap != null) *out_heap = temp_heap;
    return true;
}

fn bool scheduler_offload_write_file(SharedHandle* payload_blob, OffloadCompletion* completion) {
    char[] payload = "";
    if (!scheduler_project_shared(payload_blob, &payload)) {
        scheduler_set_offload_error(completion, "offload write-file: missing payload");
        return false;
    }

    char[] path = "";
    char[] content = "";
    if (!scheduler_split_write_file_payload(payload, &path, &content)) {
        scheduler_set_offload_error(completion, "offload write-file: invalid payload");
        return false;
    }

    char[] temp_path = "";
    char* temp_path_heap = null;
    if (!scheduler_build_atomic_temp_path(path, (usz)completion, &temp_path, &temp_path_heap, completion)) {
        return false;
    }
    defer if (temp_path_heap != null) mem::free(temp_path_heap);

    (void)omni_uv_fs_unlink((ZString)temp_path.ptr);

    int fd = omni_uv_fs_open((ZString)temp_path.ptr, 577, 0o644);
    if (fd < 0) {
        scheduler_set_offload_error_with_status(completion, "offload write-file: open temp failed", fd);
        return false;
    }

    usz written = 0;
    bool write_ok = true;
    while (written < content.len) {
        long n = omni_uv_fs_write(fd, content.ptr + written, content.len - written);
        if (n <= 0) {
            write_ok = false;
            scheduler_set_offload_error_with_status(completion, "offload write-file: write failed", (int)n);
            break;
        }
        written += (usz)n;
    }
    int close_status = omni_uv_fs_close(fd);
    if (close_status < 0 && completion.kind != OFFLOAD_RES_ERROR) {
        scheduler_set_offload_error_with_status(completion, "offload write-file: close failed", close_status);
    }

    if (!write_ok || close_status < 0) {
        (void)omni_uv_fs_unlink((ZString)temp_path.ptr);
        return false;
    }

    int rename_status = omni_uv_fs_rename((ZString)temp_path.ptr, (ZString)path.ptr);
    if (rename_status != 0) {
        (void)omni_uv_fs_unlink((ZString)temp_path.ptr);
        scheduler_set_offload_error_with_status(completion, "offload write-file: rename failed", rename_status);
        return false;
    }

    completion.kind = OFFLOAD_RES_INT;
    completion.int_result = scheduler_domain_publish_sendable_int(
        scheduler_primary_domain(),
        (long)written
    );
    return true;
}

fn bool scheduler_offload_file_exists(SharedHandle* path_blob, OffloadCompletion* completion) {
    char[] path = "";
    if (!scheduler_project_shared(path_blob, &path)) {
        scheduler_set_offload_error(completion, "offload file-exists: missing/invalid path");
        return false;
    }

    ulong size_out = 0;
    uint mode_out = 0;
    int status = omni_uv_fs_stat_basic((ZString)path.ptr, &size_out, &mode_out);

    completion.kind = OFFLOAD_RES_INT;
    completion.int_result = scheduler_domain_publish_sendable_int(
        scheduler_primary_domain(),
        status >= 0 ? 1 : 0
    );
    return true;
}

fn OffloadCompletion* scheduler_execute_offload(OffloadWork work) {
    if (work.custom_fn != null) {
        OffloadCompletion* completion = work.custom_fn(work);
        scheduler_release_shared(work.shared);
        return completion;
    }

    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) {
        scheduler_release_shared(work.shared);
        return null;
    }
    *completion = {};

    switch (work.op) {
        case OFFLOAD_SLEEP_MS:
            if (work.int_arg > 0) c_usleep((uint)(work.int_arg * 1000));
            completion.kind = OFFLOAD_RES_NIL;
        case OFFLOAD_GZIP:
            scheduler_offload_compress(true, work.shared, completion);
        case OFFLOAD_DEFLATE:
            scheduler_offload_compress(false, work.shared, completion);
        case OFFLOAD_ACCEPT_FD:
            scheduler_offload_accept_fd(work.int_arg, completion);
        case OFFLOAD_TCP_CONNECT:
            scheduler_offload_tcp_connect(work.shared, (int)work.int_arg, completion);
        case OFFLOAD_HTTP_GET:
            scheduler_offload_http_get(work.shared, completion);
        case OFFLOAD_READ_FILE:
            scheduler_offload_read_file(work.shared, completion);
        case OFFLOAD_WRITE_FILE:
            scheduler_offload_write_file(work.shared, completion);
        case OFFLOAD_FILE_EXISTS:
            scheduler_offload_file_exists(work.shared, completion);
        case OFFLOAD_TLS_CONNECT:
            scheduler_offload_tls_connect(work.int_arg, completion);
        case OFFLOAD_TLS_READ:
            scheduler_offload_tls_read(work.int_arg, work.int_arg2, completion);
        case OFFLOAD_TLS_WRITE:
            scheduler_offload_tls_write(work.int_arg, work.shared, completion);
        case OFFLOAD_TLS_CLOSE:
            scheduler_offload_tls_close(work.int_arg, completion);
    }

    scheduler_release_shared(work.shared);
    return completion;
}
