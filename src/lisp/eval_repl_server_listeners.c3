module lisp;

import std::io;
import main;

fn bool repl_server_tcp_host_is_loopback(char[] host) {
    if (host.len == 0) return false;
    return is_localhost_name(host) ||
        json_repl_str_eq(host, "127.0.0.1") ||
        json_repl_str_eq(host, "::1");
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

    while (listener_fd >= 0) {
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
    char[] host_slice = host[:repl_server_cstr_len(host)];
    if (!repl_server_tcp_host_is_loopback(host_slice)) {
        main::print_cli_error_json(
            "security/repl-tcp-loopback-required",
            "TCP REPL server must bind to localhost, 127.0.0.1, or ::1"
        );
        return 1;
    }

    char* auth_token = main::getenv("OMNI_REPL_TCP_AUTH_TOKEN");
    if (auth_token == null || auth_token[0] == 0) {
        main::print_cli_error_json(
            "security/auth-required",
            "set OMNI_REPL_TCP_AUTH_TOKEN before starting TCP REPL server"
        );
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

    while (listener_fd >= 0) {
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
        repl_server_handle_stream(
            client_fd,
            client_fd,
            true,
            "tcp",
            preload_project,
            preload_project_dir,
            true,
            auth_token
        );
        c_close_fd(client_fd);
    }

    return 0;
}
