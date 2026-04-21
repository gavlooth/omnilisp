module lisp;

import std::core::mem;
import std::io;
import main;

struct ReplServerClientThreadCtx {
    int client_fd;
    bool preload_project;
    char* preload_project_dir;
    bool auth_required;
    char* auth_token;
    bool tcp_transport;
}

fn bool repl_server_tcp_host_is_loopback(char[] host) {
    if (host.len == 0) return false;
    return is_localhost_name(host) ||
        json_repl_str_eq(host, "127.0.0.1") ||
        json_repl_str_eq(host, "::1");
}

fn void repl_server_client_thread_entry(void* ctx_ptr) {
    ReplServerClientThreadCtx* ctx = (ReplServerClientThreadCtx*)ctx_ptr;
    if (ctx == null) return;

    @pool_init(mem, 65536)
    {
        main::thread_registry_init();
        defer main::thread_registry_shutdown();

        int client_fd = ctx.client_fd;
        bool preload_project = ctx.preload_project;
        char* preload_project_dir = ctx.preload_project_dir;
        bool auth_required = ctx.auth_required;
        char* auth_token = ctx.auth_token;
        bool tcp_transport = ctx.tcp_transport;
        mem::free(ctx);

        repl_server_handle_stream(
            client_fd,
            client_fd,
            true,
            tcp_transport ? "tcp" : "unix",
            preload_project,
            preload_project_dir,
            auth_required,
            auth_token
        );
        c_close_fd(client_fd);
    };
}

fn bool repl_server_start_client_thread(
    int client_fd,
    bool tcp_transport,
    bool preload_project = false,
    char* preload_project_dir = null,
    bool auth_required = false,
    char* auth_token = null
) {
    if (client_fd < 0) return false;
    ReplServerClientThreadCtx* ctx = (ReplServerClientThreadCtx*)mem::malloc(ReplServerClientThreadCtx.sizeof);
    if (ctx == null) return false;
    *ctx = {
        .client_fd = client_fd,
        .preload_project = preload_project,
        .preload_project_dir = preload_project_dir,
        .auth_required = auth_required,
        .auth_token = auth_token,
        .tcp_transport = tcp_transport,
    };

    void* thread_handle = omni_uv_thread_new();
    if (thread_handle == null) {
        mem::free(ctx);
        return false;
    }
    if (omni_uv_thread_start(thread_handle, &repl_server_client_thread_entry, ctx) != 0) {
        omni_uv_thread_free(thread_handle);
        mem::free(ctx);
        return false;
    }

    int detach_status = omni_uv_thread_detach_handle(thread_handle);
    if (detach_status == 0) {
        omni_uv_thread_free(thread_handle);
    } else {
        io::eprintfn("WARNING: REPL server client handler detach failed; thread handle retained by OS until exit");
    }
    return true;
}

fn void repl_server_reject_client_start_failure(int client_fd, bool socket_write) {
    if (client_fd < 0) return;
    ReplServerOutput output = {};
    repl_server_output_init(&output, client_fd, socket_write);
    defer repl_server_output_destroy(&output);
    (void)repl_server_send_protocol_error(
        &output,
        "",
        "runtime/client-handler-start-failed",
        "failed to start repl client handler"
    );
}

fn int repl_server_socket(char* socket_path, bool preload_project = false, char* preload_project_dir = null) {
    if (socket_path == null || socket_path[0] == 0) {
        main::repl_emit_cli_usage_json("missing socket path for --repl-server");
        return 1;
    }

    main::thread_registry_init();
    defer main::thread_registry_shutdown();

    int listener_fd = -1;
    if (omni_unix_socket_listen_fd(socket_path, REPL_SERVER_LISTEN_BACKLOG, &listener_fd) < 0 || listener_fd < 0) {
        char[320] msg_buf;
        if (try msg = io::bprintf(&msg_buf, "failed to listen on REPL socket %s", (ZString)socket_path)) {
            main::repl_emit_cli_error_json("io/listen-failed", msg);
        } else {
            main::repl_emit_cli_error_json("io/listen-failed", "failed to listen on REPL socket");
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
                main::repl_emit_cli_error_json("io/accept-failed", msg);
            } else {
                main::repl_emit_cli_error_json("io/accept-failed", "failed to accept client on REPL socket");
            }
            return 1;
        }
        if (!repl_server_start_client_thread(
            client_fd,
            false,
            preload_project,
            preload_project_dir
        )) {
            repl_server_reject_client_start_failure(client_fd, true);
            c_close_fd(client_fd);
        }
    }

    return 0;
}

fn int repl_server_tcp(char* host, int port, bool preload_project = false, char* preload_project_dir = null) {
    if (host == null || host[0] == 0 || port <= 0 || port > 65535) {
        main::repl_emit_cli_usage_json("invalid TCP host/port for --repl-server");
        return 1;
    }
    char[] host_slice = host[:repl_server_cstr_len(host)];
    if (!repl_server_tcp_host_is_loopback(host_slice)) {
        main::repl_emit_cli_error_json(
            "security/repl-tcp-loopback-required",
            "TCP REPL server must bind to localhost, 127.0.0.1, or ::1"
        );
        return 1;
    }

    char* auth_token = main::getenv("OMNI_REPL_TCP_AUTH_TOKEN");
    if (auth_token == null || auth_token[0] == 0) {
        main::repl_emit_cli_error_json(
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
            main::repl_emit_cli_error_json("io/listen-failed", msg);
        } else {
            main::repl_emit_cli_error_json("io/listen-failed", "failed to listen on TCP REPL socket");
        }
        return 1;
    }
    defer if (listener_fd >= 0) c_close_fd(listener_fd);

    if (!repl_server_publish_tcp_port_file(port)) {
        main::repl_emit_cli_error_json("io/write-failed", "failed to write .omni-repl-port for TCP REPL server");
        return 1;
    }

    while (listener_fd >= 0) {
        int client_fd = c_accept(listener_fd, null, null);
        if (client_fd < 0) {
            char[320] msg_buf;
            if (try msg = io::bprintf(&msg_buf, "failed to accept client on TCP REPL socket %s:%d", (ZString)host, port)) {
                main::repl_emit_cli_error_json("io/accept-failed", msg);
            } else {
                main::repl_emit_cli_error_json("io/accept-failed", "failed to accept client on TCP REPL socket");
            }
            return 1;
        }
        if (!repl_server_start_client_thread(
            client_fd,
            true,
            preload_project,
            preload_project_dir,
            true,
            auth_token
        )) {
            repl_server_reject_client_start_failure(client_fd, true);
            c_close_fd(client_fd);
        }
    }

    return 0;
}
