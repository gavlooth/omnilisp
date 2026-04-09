module lisp;

import main;

extern fn int omni_unix_socket_listen_fd(char* path, int backlog, int* out_fd) @extern("omni_unix_socket_listen_fd");
const usz REPL_SERVER_LINE_MAX = 65536;
const usz REPL_SERVER_SESSION_ID_MAX = 32;
const int REPL_SERVER_LISTEN_BACKLOG = 16;
const int REPL_SERVER_SEND_FLAGS = 0x4000; // MSG_NOSIGNAL on Linux

fn void repl_server_handle_stream(
    int read_fd,
    int write_fd,
    bool socket_write,
    char[] transport_name,
    bool preload_project = false,
    char* preload_project_dir = null,
    bool auth_required = false,
    char* auth_token = null
) {
    ReplServerConnection conn = {};
    repl_server_connection_init(
        &conn,
        write_fd,
        socket_write,
        preload_project,
        preload_project_dir,
        auth_required,
        auth_token
    );
    defer repl_server_connection_destroy(&conn);

    if (!repl_server_worker_start(&conn.worker)) {
        (void)repl_server_send_protocol_error(&conn.output, "", "runtime/server-init-failed", "failed to start repl worker");
        return;
    }
    defer repl_server_worker_shutdown(&conn.worker);

    char[REPL_SERVER_LINE_MAX] line_buf = {};
    while (true) {
        ReplServerReadResult read = repl_server_read_line_fd(read_fd, line_buf[..], socket_write);
        if (read.failed) break;
        if (read.len == 0 && read.eof) break;
        if (read.truncated) {
            if (!repl_server_send_protocol_error(&conn.output, "", "protocol/request-too-large", "input line exceeds supported length")) break;
            continue;
        }
        if (read.len == 0) continue;

        ReplServerRequest request = repl_server_parse_request(line_buf[:read.len]);
        defer repl_server_request_free(&request);
        if (!request.ok) {
            char[] id = request.has_id ? request.id : "";
            if (!repl_server_send_protocol_error(&conn.output, id, "protocol/invalid-request", request.error_message)) break;
            continue;
        }

        if (!repl_server_handle_request(&conn, &request, transport_name)) break;
    }
}

fn int repl_server_stdio(bool preload_project = false, char* preload_project_dir = null) {
    main::thread_registry_init();
    defer main::thread_registry_shutdown();
    repl_server_handle_stream(0, 1, false, "stdio", preload_project, preload_project_dir);
    return 0;
}
