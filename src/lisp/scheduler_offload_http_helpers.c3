module lisp;

const usz HTTP_OFFLOAD_INITIAL_RESP_CAP = 16384;

fn bool scheduler_send_all_fd(int fd, char* buf, usz len) {
    usz sent = 0;
    while (sent < len) {
        long written = c_send(fd, buf + sent, len - sent, 0);
        if (written <= 0) return false;
        sent += (usz)written;
    }
    return true;
}

fn void scheduler_discard_offload_response_buffer(char** buf_ref, usz* len_ref, usz* cap_ref) {
    if (*buf_ref != null) {
        mem::free(*buf_ref);
        *buf_ref = null;
    }
    if (len_ref != null) *len_ref = 0;
    if (cap_ref != null) *cap_ref = 0;
}

fn bool scheduler_reserve_offload_response_buffer(char** buf_ref, usz* cap_ref, usz needed, OffloadCompletion* completion) {
    if (needed <= *cap_ref) return true;

    usz new_cap = *cap_ref == 0 ? HTTP_OFFLOAD_INITIAL_RESP_CAP : *cap_ref;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            scheduler_discard_offload_response_buffer(buf_ref, null, cap_ref);
            scheduler_set_offload_error(completion, "offload http-get: response too large");
            return false;
        }
        new_cap *= 2;
    }

    char* grown = *buf_ref == null
        ? (char*)mem::malloc(new_cap + 1)
        : (char*)mem::realloc(*buf_ref, new_cap + 1);
    if (grown == null) {
        scheduler_discard_offload_response_buffer(buf_ref, null, cap_ref);
        scheduler_set_offload_error(completion, "offload http-get: out of memory");
        return false;
    }

    *buf_ref = grown;
    *cap_ref = new_cap;
    return true;
}

fn bool scheduler_append_offload_response_chunk(
    char** buf_ref,
    usz* len_ref,
    usz* cap_ref,
    char* chunk,
    usz chunk_len,
    OffloadCompletion* completion
) {
    if (chunk_len == 0) return true;
    if (chunk_len > usz.max - *len_ref) {
        scheduler_discard_offload_response_buffer(buf_ref, len_ref, cap_ref);
        scheduler_set_offload_error(completion, "offload http-get: response too large");
        return false;
    }

    usz next_len = *len_ref + chunk_len;
    if (!scheduler_reserve_offload_response_buffer(buf_ref, cap_ref, next_len, completion)) {
        if (len_ref != null) *len_ref = 0;
        return false;
    }

    for (usz i = 0; i < chunk_len; i++) (*buf_ref)[*len_ref + i] = chunk[i];
    *len_ref = next_len;
    return true;
}

fn bool scheduler_publish_offload_response_owned(OffloadCompletion* completion, char* resp_buf, usz resp_len) {
    if (resp_buf == null) {
        resp_buf = (char*)mem::malloc(1);
        if (resp_buf == null) {
            scheduler_set_offload_error(completion, "offload http-get: out of memory");
            return false;
        }
    }

    resp_buf[resp_len] = 0;
    if (scheduler_set_offload_bytes_owned(
            completion,
            resp_buf,
            resp_len,
            "offload http-get: result allocation failed"
        )) {
        return true;
    }

    mem::free(resp_buf);
    return false;
}
