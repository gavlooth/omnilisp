module lisp;

import std::io;
import main;

extern fn int omni_unix_socket_listen_fd(char* path, int backlog, int* out_fd) @extern("omni_unix_socket_listen_fd");
const usz REPL_SERVER_LINE_MAX = 65536;
const usz REPL_SERVER_SESSION_ID_MAX = 32;
const int REPL_SERVER_LISTEN_BACKLOG = 16;
const int REPL_SERVER_SEND_FLAGS = 0x4000; // MSG_NOSIGNAL on Linux

fn bool repl_server_handle_request(ReplServerConnection* conn, ReplServerRequest* request, char[] transport_name) {
    ReplServerOutput* output = &conn.output;
    ReplServerWorker* worker = &conn.worker;
    char[] id = request.id;

    if (json_repl_str_eq(request.op, "describe")) {
        if (!repl_server_send_describe(output, id, transport_name)) return false;
        return repl_server_send_done(output, id, "");
    }

    if (json_repl_str_eq(request.op, "clone")) {
        if (repl_server_worker_busy(worker)) {
            return repl_server_send_protocol_error(output, id, "protocol/server-busy", "repl server is busy");
        }
        ReplServerSession* session = repl_server_new_session(conn);
        if (session == null) {
            return repl_server_send_protocol_error(output, id, "runtime/session-create-failed", "failed to create session");
        }
        if (!repl_server_worker_submit(worker, REPL_SERVER_CMD_CLONE, session, id)) {
            repl_server_close_session(session);
            return repl_server_send_protocol_error(output, id, "protocol/server-busy", "repl server is busy");
        }
        return true;
    }

    if (!request.has_session) {
        return repl_server_send_protocol_error(output, id, "protocol/missing-session", "request field 'session' is required for this operation");
    }

    ReplServerSession* session = repl_server_find_session(conn, request.session);
    if (session == null || !session.active) {
        return repl_server_send_protocol_error(output, id, "protocol/unknown-session", "unknown session");
    }

    if (json_repl_str_eq(request.op, "interrupt")) {
        if (!request.has_target) {
            return repl_server_send_protocol_error(output, id, "protocol/missing-target", "request field 'target' is required for interrupt");
        }
        if (!repl_server_worker_request_interrupt(worker, session, request.target)) {
            return repl_server_send_protocol_error(output, id, "protocol/no-such-target", "interrupt target is not currently running");
        }
        return repl_server_send_done(output, id, session.id_ptr[:session.id_len]);
    }

    if (json_repl_str_eq(request.op, "stdin")) {
        if (!request.has_target) {
            return repl_server_send_protocol_error(output, id, "protocol/missing-target", "request field 'target' is required for stdin");
        }
        if (!request.has_data && (!request.has_eof || !request.eof)) {
            return repl_server_send_protocol_error(output, id, "protocol/missing-data", "request field 'data' or eof=true is required for stdin");
        }
        if (!repl_server_worker_accepts_stdin(worker, session, request.target)) {
            return repl_server_send_protocol_error(output, id, "protocol/no-such-target", "stdin target is not currently running");
        }
        if (request.has_data && request.data.len > 0 && !io_input_state_append(&session.input, request.data)) {
            return repl_server_send_protocol_error(output, id, "runtime/stdin-route-failed", "failed to route stdin data to target session");
        }
        if (request.has_eof && request.eof && !io_input_state_close(&session.input)) {
            return repl_server_send_protocol_error(output, id, "runtime/stdin-route-failed", "failed to close routed stdin for target session");
        }
        return repl_server_send_done(output, id, session.id_ptr[:session.id_len]);
    }

    if (repl_server_worker_busy(worker)) {
        return repl_server_send_protocol_error(output, id, "protocol/server-busy", "repl server is busy");
    }

    if (json_repl_str_eq(request.op, "close")) {
        return repl_server_worker_submit(worker, REPL_SERVER_CMD_CLOSE, session, id)
            ? true
            : repl_server_send_protocol_error(output, id, "protocol/server-busy", "repl server is busy");
    }

    if (!session.ready || session.interp == null) {
        return repl_server_send_protocol_error(output, id, "protocol/session-not-ready", "session is not ready");
    }

    if (json_repl_str_eq(request.op, "eval")) {
        if (!request.has_code) {
            return repl_server_send_protocol_error(output, id, "protocol/missing-code", "request field 'code' is required for eval");
        }
        return repl_server_worker_submit(worker, REPL_SERVER_CMD_EVAL, session, id, request.code, request.program_mode)
            ? true
            : repl_server_send_protocol_error(output, id, "protocol/server-busy", "repl server is busy");
    }

    if (json_repl_str_eq(request.op, "load-file")) {
        if (!request.has_path) {
            return repl_server_send_protocol_error(output, id, "protocol/missing-path", "request field 'path' is required for load-file");
        }
        return repl_server_worker_submit(worker, REPL_SERVER_CMD_LOAD_FILE, session, id, request.path)
            ? true
            : repl_server_send_protocol_error(output, id, "protocol/server-busy", "repl server is busy");
    }

    if (json_repl_str_eq(request.op, "complete")) {
        if (!request.has_prefix) {
            return repl_server_send_protocol_error(output, id, "protocol/missing-prefix", "request field 'prefix' is required for complete");
        }
        return repl_server_worker_submit(worker, REPL_SERVER_CMD_COMPLETE, session, id, request.prefix)
            ? true
            : repl_server_send_protocol_error(output, id, "protocol/server-busy", "repl server is busy");
    }

    return repl_server_send_protocol_error(output, id, "protocol/unsupported-op", "unsupported operation");
}

fn void repl_server_handle_stream(
    int read_fd,
    int write_fd,
    bool socket_write,
    char[] transport_name,
    bool preload_project = false,
    char* preload_project_dir = null
) {
    ReplServerConnection conn = {};
    repl_server_connection_init(&conn, write_fd, socket_write, preload_project, preload_project_dir);
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

fn int repl_server_socket(char* socket_path, bool preload_project = false, char* preload_project_dir = null) {
    if (socket_path == null || socket_path[0] == 0) {
        main::print_cli_usage_json("missing socket path for --repl-server");
        return 1;
    }

    main::thread_registry_init();
    defer main::thread_registry_shutdown();

    int listener_fd = -1;
    if (omni_unix_socket_listen_fd(socket_path, REPL_SERVER_LISTEN_BACKLOG, &listener_fd) < 0 || listener_fd < 0) {
        char[320] msg_buf;
        if (try msg = io::bprintf(&msg_buf, "failed to listen on REPL socket %s", (ZString)socket_path)) {
            main::print_cli_error_json("io/listen-failed", msg);
        } else {
            main::print_cli_error_json("io/listen-failed", "failed to listen on REPL socket");
        }
        return 1;
    }
    defer {
        if (listener_fd >= 0) c_close_fd(listener_fd);
        (void)omni_uv_fs_unlink(socket_path);
    }

    while (true) {
        int client_fd = c_accept(listener_fd, null, null);
        if (client_fd < 0) {
            char[320] msg_buf;
            if (try msg = io::bprintf(&msg_buf, "failed to accept client on REPL socket %s", (ZString)socket_path)) {
                main::print_cli_error_json("io/accept-failed", msg);
            } else {
                main::print_cli_error_json("io/accept-failed", "failed to accept client on REPL socket");
            }
            return 1;
        }
        repl_server_handle_stream(client_fd, client_fd, true, "unix", preload_project, preload_project_dir);
        c_close_fd(client_fd);
    }

    return 0;
}

fn int repl_server_tcp(char* host, int port, bool preload_project = false, char* preload_project_dir = null) {
    if (host == null || host[0] == 0 || port <= 0 || port > 65535) {
        main::print_cli_usage_json("invalid TCP host/port for --repl-server");
        return 1;
    }

    main::thread_registry_init();
    defer main::thread_registry_shutdown();

    int listener_fd = -1;
    if (omni_uv_tcp_listen_fd(host, port, REPL_SERVER_LISTEN_BACKLOG, &listener_fd) != 0 || listener_fd < 0) {
        char[320] msg_buf;
        if (try msg = io::bprintf(&msg_buf, "failed to listen on TCP REPL socket %s:%d", (ZString)host, port)) {
            main::print_cli_error_json("io/listen-failed", msg);
        } else {
            main::print_cli_error_json("io/listen-failed", "failed to listen on TCP REPL socket");
        }
        return 1;
    }
    defer if (listener_fd >= 0) c_close_fd(listener_fd);

    if (!repl_server_publish_tcp_port_file(port)) {
        main::print_cli_error_json("io/write-failed", "failed to write .omni-repl-port for TCP REPL server");
        return 1;
    }

    while (true) {
        int client_fd = c_accept(listener_fd, null, null);
        if (client_fd < 0) {
            char[320] msg_buf;
            if (try msg = io::bprintf(&msg_buf, "failed to accept client on TCP REPL socket %s:%d", (ZString)host, port)) {
                main::print_cli_error_json("io/accept-failed", msg);
            } else {
                main::print_cli_error_json("io/accept-failed", "failed to accept client on TCP REPL socket");
            }
            return 1;
        }
        repl_server_handle_stream(client_fd, client_fd, true, "tcp", preload_project, preload_project_dir);
        c_close_fd(client_fd);
    }

    return 0;
}
