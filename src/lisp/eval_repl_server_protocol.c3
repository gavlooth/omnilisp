module lisp;

import std::core::mem;

struct ReplServerReadResult {
    usz len;
    bool eof;
    bool truncated;
    bool failed;
}

struct ReplServerRequest {
    bool ok;
    bool has_id;
    bool has_op;
    bool has_auth;
    bool has_session;
    bool has_target;
    bool has_code;
    bool has_path;
    bool has_prefix;
    bool has_data;
    bool has_eof;
    bool eof;
    bool program_mode;
    char[] id;
    char[] op;
    char[] auth;
    char[] session;
    char[] target;
    char[] code;
    char[] path;
    char[] prefix;
    char[] data;
    char[] error_message;
}

fn ReplServerReadResult repl_server_read_line_fd(int fd, char[] buffer, bool socket_read) {
    ReplServerReadResult out = { .len = 0, .eof = false, .truncated = false, .failed = false };
    usz max_len = buffer.len - 1;
    char ch = 0;

    while (out.len < max_len) {
        long nread = socket_read ? c_recv(fd, &ch, 1, 0) : omni_uv_fs_read(fd, &ch, 1);
        if (nread == 0) {
            out.eof = true;
            break;
        }
        if (nread < 0) {
            out.failed = true;
            break;
        }
        if (ch == '\n') break;
        buffer[out.len] = ch;
        out.len++;
    }

    if (!out.eof && !out.failed && out.len >= max_len) {
        while (true) {
            long nread = socket_read ? c_recv(fd, &ch, 1, 0) : omni_uv_fs_read(fd, &ch, 1);
            if (nread == 0) {
                out.eof = true;
                break;
            }
            if (nread < 0) {
                out.failed = true;
                break;
            }
            if (ch == '\n') break;
            out.truncated = true;
        }
    }

    buffer[out.len] = 0;
    return out;
}

fn void repl_server_request_free(ReplServerRequest* request) {
    if (request == null) return;
    if (request.has_id && request.id.ptr != null) mem::free(request.id.ptr);
    if (request.has_op && request.op.ptr != null) mem::free(request.op.ptr);
    if (request.has_auth && request.auth.ptr != null) mem::free(request.auth.ptr);
    if (request.has_session && request.session.ptr != null) mem::free(request.session.ptr);
    if (request.has_target && request.target.ptr != null) mem::free(request.target.ptr);
    if (request.has_code && request.code.ptr != null) mem::free(request.code.ptr);
    if (request.has_path && request.path.ptr != null) mem::free(request.path.ptr);
    if (request.has_prefix && request.prefix.ptr != null) mem::free(request.prefix.ptr);
    if (request.has_data && request.data.ptr != null) mem::free(request.data.ptr);
    *request = {};
}
