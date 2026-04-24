module lisp;

import std::io;

fn void run_async_repl_server_worker_start_requires_ready_primitives_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    ReplServerWorker worker = {};
    bool started = repl_server_worker_start(&worker);
    bool ok = !started && !worker.thread_started && worker.thread_handle == null;
    if (ok) {
        emit_pass_literal("[PASS] async repl-server worker start requires ready sync primitives");
        (*pass)++;
    } else {
        io::printn("[FAIL] async repl-server worker start requires ready sync primitives");
        (*fail)++;
    }
}

fn void run_async_repl_server_worker_destroy_releases_sync_primitives_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    ReplServerWorker worker = {};
    repl_server_worker_init(&worker, null, null);
    bool initialized = worker.mu_ready && worker.cv_ready;
    repl_server_worker_destroy(&worker);
    bool ok = initialized && !worker.mu_ready && !worker.cv_ready &&
        worker.thread_handle == null && !worker.thread_started;
    if (ok) {
        emit_pass_literal("[PASS] async repl-server worker destroy releases sync primitives");
        (*pass)++;
    } else {
        io::printn("[FAIL] async repl-server worker destroy releases sync primitives");
        (*fail)++;
    }
}

fn void run_async_repl_server_pending_interrupt_window_test(Interp* interp, int* pass, int* fail) {
    ReplServerConnection conn = {};
    repl_server_connection_init(&conn, 1, false);
    defer repl_server_connection_destroy(&conn);

    if (!conn.worker.mu_ready || !conn.worker.cv_ready) {
        io::printn("[FAIL] async repl-server pending interrupt window (worker init)");
        (*fail)++;
        return;
    }

    ReplServerSession* session = repl_server_new_session(&conn);
    if (session == null) {
        io::printn("[FAIL] async repl-server pending interrupt window (session alloc)");
        (*fail)++;
        return;
    }

    char* request_id = null;
    usz request_id_len = 0;
    if (!repl_server_dup_heap_string("42", &request_id, &request_id_len)) {
        io::printn("[FAIL] async repl-server pending interrupt window (id alloc)");
        (*fail)++;
        return;
    }

    conn.worker.pending_command = {
        .kind = REPL_SERVER_CMD_EVAL,
        .session = session,
        .id_ptr = request_id,
        .id_len = request_id_len,
    };
    conn.worker.command_pending = true;

    bool accepted = repl_server_worker_request_interrupt(&conn.worker, session, "42");
    bool flag_clear = session.interrupt_requested.load() == 0;
    if (!accepted && flag_clear) {
        emit_pass_literal("[PASS] async repl-server pending interrupt window");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] async repl-server pending interrupt window (accepted=%s interrupt_flag=%d)",
            accepted ? "true" : "false",
            session.interrupt_requested.load()
        );
        (*fail)++;
    }
}

fn void run_async_repl_server_active_interrupt_test(Interp* interp, int* pass, int* fail) {
    ReplServerConnection conn = {};
    repl_server_connection_init(&conn, 1, false);
    defer repl_server_connection_destroy(&conn);

    if (!conn.worker.mu_ready || !conn.worker.cv_ready) {
        io::printn("[FAIL] async repl-server active interrupt (worker init)");
        (*fail)++;
        return;
    }

    ReplServerSession* session = repl_server_new_session(&conn);
    if (session == null) {
        io::printn("[FAIL] async repl-server active interrupt (session alloc)");
        (*fail)++;
        return;
    }

    char[3] request_id_buf = {'4', '2', 0};
    conn.worker.busy = true;
    conn.worker.active_kind = REPL_SERVER_CMD_EVAL;
    conn.worker.active_session = session;
    conn.worker.active_request_id_ptr = &request_id_buf[0];
    conn.worker.active_request_id_len = 2;

    bool accepted = repl_server_worker_request_interrupt(&conn.worker, session, "42");
    bool flag_set = session.interrupt_requested.load() != 0;
    if (accepted && flag_set) {
        emit_pass_literal("[PASS] async repl-server active interrupt");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] async repl-server active interrupt (accepted=%s interrupt_flag=%d)",
            accepted ? "true" : "false",
            session.interrupt_requested.load()
        );
        (*fail)++;
    }
}

fn void run_async_repl_server_request_queue_accepts_multiple_pending_test(Interp* interp, int* pass, int* fail) {
    ReplServerConnection conn = {};
    repl_server_connection_init(&conn, 1, false);
    defer repl_server_connection_destroy(&conn);

    if (!conn.worker.mu_ready || !conn.worker.cv_ready) {
        io::printn("[FAIL] async repl-server request queue accepts multiple pending commands (worker init)");
        (*fail)++;
        return;
    }

    ReplServerSession* session = repl_server_new_session(&conn);
    if (session == null) {
        io::printn("[FAIL] async repl-server request queue accepts multiple pending commands (session alloc)");
        (*fail)++;
        return;
    }

    bool submit_a = repl_server_worker_submit(&conn.worker, REPL_SERVER_CMD_EVAL, session, "qa", "(+ 1 1)");
    bool submit_b = repl_server_worker_submit(&conn.worker, REPL_SERVER_CMD_LOAD_FILE, session, "qb", "demo.omni");
    bool queue_size_ok = conn.worker.command_queue_count == 2;
    bool busy_ok = !repl_server_worker_busy(&conn.worker);
    bool stdin_a = repl_server_worker_accepts_stdin(&conn.worker, session, "qa");
    bool stdin_b = repl_server_worker_accepts_stdin(&conn.worker, session, "qb");
    bool interrupt_queued = repl_server_worker_request_interrupt(&conn.worker, session, "qa");
    bool interrupt_ok = !interrupt_queued && session.interrupt_requested.load() == 0;

    bool order_ok = queue_size_ok &&
        conn.worker.command_queue[conn.worker.command_queue_head].id_len == 2 &&
        conn.worker.command_queue[conn.worker.command_queue_head].id_ptr[1] == 'a' &&
        conn.worker.command_queue[(conn.worker.command_queue_head + 1) %
            REPL_SERVER_WORKER_QUEUE_CAPACITY].id_len == 2 &&
        conn.worker.command_queue[(conn.worker.command_queue_head + 1) %
            REPL_SERVER_WORKER_QUEUE_CAPACITY].id_ptr[1] == 'b';

    bool ok = submit_a && submit_b && queue_size_ok && busy_ok && stdin_a && stdin_b && interrupt_ok && order_ok;
    if (ok) {
        emit_pass_literal("[PASS] async repl-server request queue accepts multiple pending commands");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] async repl-server request queue accepts multiple pending commands (submit_a=%s submit_b=%s count=%d busy_ok=%s stdin_a=%s stdin_b=%s interrupt_ok=%s order_ok=%s)",
            submit_a ? "yes" : "no",
            submit_b ? "yes" : "no",
            (long)conn.worker.command_queue_count,
            busy_ok ? "yes" : "no",
            stdin_a ? "yes" : "no",
            stdin_b ? "yes" : "no",
            interrupt_ok ? "yes" : "no",
            order_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_async_repl_server_request_queue_backpressure_test(Interp* interp, int* pass, int* fail) {
    ReplServerConnection conn = {};
    repl_server_connection_init(&conn, 1, false);
    defer repl_server_connection_destroy(&conn);

    if (!conn.worker.mu_ready || !conn.worker.cv_ready) {
        io::printn("[FAIL] async repl-server request queue backpressure (worker init)");
        (*fail)++;
        return;
    }

    ReplServerSession* session = repl_server_new_session(&conn);
    if (session == null) {
        io::printn("[FAIL] async repl-server request queue backpressure (session alloc)");
        (*fail)++;
        return;
    }

    bool filled = true;
    for (usz i = 0; i < REPL_SERVER_WORKER_QUEUE_CAPACITY; i++) {
        char[16] id_buf;
        char[] id;
        if (try rendered = io::bprintf(&id_buf, "q%d", (long)i)) {
            id = rendered;
        } else {
            filled = false;
            break;
        }
        if (!repl_server_worker_submit(&conn.worker, REPL_SERVER_CMD_EVAL, session, id, "(+ 1 1)")) {
            filled = false;
            break;
        }
    }

    bool full_busy = repl_server_worker_busy(&conn.worker);
    bool overflow_rejected = !repl_server_worker_submit(
        &conn.worker,
        REPL_SERVER_CMD_EVAL,
        session,
        "overflow",
        "(+ 2 2)"
    );
    bool count_ok = conn.worker.command_queue_count == REPL_SERVER_WORKER_QUEUE_CAPACITY;

    bool ok = filled && full_busy && overflow_rejected && count_ok;
    if (ok) {
        emit_pass_literal("[PASS] async repl-server request queue backpressure");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] async repl-server request queue backpressure (filled=%s full_busy=%s overflow_rejected=%s count=%d)",
            filled ? "yes" : "no",
            full_busy ? "yes" : "no",
            overflow_rejected ? "yes" : "no",
            (long)conn.worker.command_queue_count
        );
        (*fail)++;
    }
}

fn void run_async_repl_server_clone_queue_defers_session_allocation_test(Interp* interp, int* pass, int* fail) {
    ReplServerConnection conn = {};
    repl_server_connection_init(&conn, 1, false);
    defer repl_server_connection_destroy(&conn);

    if (!conn.worker.mu_ready || !conn.worker.cv_ready) {
        io::printn("[FAIL] async repl-server clone queue defers session allocation (worker init)");
        (*fail)++;
        return;
    }

    bool submit_clone = repl_server_worker_submit(&conn.worker, REPL_SERVER_CMD_CLONE, null, "clone-a");
    bool no_session_alloc = conn.session_capacity == 0 && conn.sessions == null;
    bool queued_null_session = submit_clone &&
        conn.worker.command_queue_count == 1 &&
        conn.worker.command_queue[conn.worker.command_queue_head].session == null;

    bool ok = submit_clone && no_session_alloc && queued_null_session;
    if (ok) {
        emit_pass_literal("[PASS] async repl-server clone queue defers session allocation");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] async repl-server clone queue defers session allocation (submit=%s capacity=%d queued_null=%s)",
            submit_clone ? "yes" : "no",
            (long)conn.session_capacity,
            queued_null_session ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_async_repl_server_input_destroy_close_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    InterpInputState state = {};
    io_input_state_init(&state);
    defer io_input_state_destroy(&state);

    bool init_ok = state.mu_ready && state.cv_ready;
    bool append_ok = io_input_state_append(&state, "abc");
    io_input_state_destroy(&state);

    char[8] line_buf = {};
    IoInputReadResult read = io_input_state_read_line(&state, line_buf[0..7], null);
    bool post_destroy_ok = state.closed &&
                           state.buf == null &&
                           state.len == 0 &&
                           state.cap == 0 &&
                           !state.mu_ready &&
                           !state.cv_ready &&
                           read.status == IO_INPUT_READ_FAILED &&
                           read.len == 0;

    if (init_ok && append_ok && post_destroy_ok) {
        emit_pass_literal("[PASS] async repl-server input destroy closes state");
        (*pass)++;
    } else {
        io::printn("[FAIL] async repl-server input destroy closes state");
        (*fail)++;
    }
}

fn void run_async_repl_server_auth_policy_test(Interp* interp, int* pass, int* fail) {
    ReplServerConnection open_conn = {};
    repl_server_connection_init(&open_conn, 1, false);
    defer repl_server_connection_destroy(&open_conn);

    ReplServerRequest open_eval = {
        .has_op = true,
        .op = "eval",
    };
    if (!repl_server_request_is_authorized(&open_conn, &open_eval)) {
        io::printn("[FAIL] async repl-server auth policy (open transport)");
        (*fail)++;
        return;
    }

    char[7] token = {'s', 'e', 'c', 'r', 'e', 't', 0};
    ReplServerConnection secured_conn = {};
    repl_server_connection_init(&secured_conn, 1, false, false, null, true, &token[0]);
    defer repl_server_connection_destroy(&secured_conn);

    ReplServerRequest describe = {
        .has_op = true,
        .op = "describe",
    };
    ReplServerRequest missing_auth = {
        .has_op = true,
        .op = "eval",
    };
    ReplServerRequest wrong_auth = {
        .has_op = true,
        .op = "eval",
        .has_auth = true,
        .auth = "nope",
    };
    ReplServerRequest correct_auth = {
        .has_op = true,
        .op = "eval",
        .has_auth = true,
        .auth = "secret",
    };

    bool ok =
        repl_server_request_is_authorized(&secured_conn, &describe) &&
        !repl_server_request_is_authorized(&secured_conn, &missing_auth) &&
        !repl_server_request_is_authorized(&secured_conn, &wrong_auth) &&
        repl_server_request_is_authorized(&secured_conn, &correct_auth) &&
        repl_server_tcp_host_is_loopback("127.0.0.1") &&
        repl_server_tcp_host_is_loopback("localhost") &&
        repl_server_tcp_host_is_loopback("::1") &&
        !repl_server_tcp_host_is_loopback("0.0.0.0");

    if (ok) {
        emit_pass_literal("[PASS] async repl-server auth policy requires token on secured transports");
        (*pass)++;
    } else {
        io::printn("[FAIL] async repl-server auth policy requires token on secured transports");
        (*fail)++;
    }
}

fn bool repl_server_test_socketpair(int* left, int* right) {
    int[2] fds = {};
    if (c_socketpair(AF_UNIX, SOCK_STREAM, 0, &fds[0]) != 0) return false;
    *left = fds[0];
    *right = fds[1];
    return true;
}

fn bool repl_server_test_send_describe_and_read(int fd, char[] id, char[] transport_name) {
    char[128] request_buf;
    char[] request;
    if (try rendered = io::bprintf(&request_buf, "{\"id\":\"%s\",\"op\":\"describe\"}\n", (ZString)id.ptr)) {
        request = rendered;
    } else {
        return false;
    }
    if (!repl_server_write_all_raw(fd, request, true)) return false;

    char[4096] response_buf = {};
    long nread = c_recv(fd, &response_buf[0], response_buf.len, 0);
    if (nread <= 0) return false;
    char[] response = response_buf[:(usz)nread];
    return str_contains(response, "\"event\":\"describe\"") &&
        str_contains(response, "\"id\":\"") &&
        str_contains(response, id) &&
        str_contains(response, transport_name);
}

fn void run_async_repl_server_client_threads_handle_independent_streams_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    int server_a = -1;
    int client_a = -1;
    void* client_thread_a = null;
    int server_b = -1;
    int client_b = -1;
    void* client_thread_b = null;
    if (!repl_server_test_socketpair(&server_a, &client_a) ||
        !repl_server_test_socketpair(&server_b, &client_b)) {
        if (server_a >= 0) c_close_fd(server_a);
        if (client_a >= 0) c_close_fd(client_a);
        if (server_b >= 0) c_close_fd(server_b);
        if (client_b >= 0) c_close_fd(client_b);
        io::printn("[FAIL] async repl-server client threads handle independent streams (socketpair)");
        (*fail)++;
        return;
    }

    bool started_a = repl_server_start_joinable_client_thread(server_a, false, &client_thread_a);
    bool started_b = repl_server_start_joinable_client_thread(server_b, true, &client_thread_b);
    if (!started_a) c_close_fd(server_a);
    if (!started_b) c_close_fd(server_b);

    bool response_a = started_a && repl_server_test_send_describe_and_read(client_a, "a1", "unix");
    bool response_b = started_b && repl_server_test_send_describe_and_read(client_b, "b1", "tcp");

    c_close_fd(client_a);
    c_close_fd(client_b);
    if (client_thread_a != null) repl_server_join_client_thread(client_thread_a);
    if (client_thread_b != null) repl_server_join_client_thread(client_thread_b);

    bool ok = started_a && started_b && response_a && response_b;
    if (ok) {
        emit_pass_literal("[PASS] async repl-server client threads handle independent streams");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] async repl-server client threads handle independent streams (started_a=%s started_b=%s response_a=%s response_b=%s)",
            started_a ? "yes" : "no",
            started_b ? "yes" : "no",
            response_a ? "yes" : "no",
            response_b ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_async_repl_server_client_start_failure_rejects_protocol_error_test(Interp* interp, int* pass, int* fail) {
    (void)interp;

    int left = -1;
    int right = -1;
    bool ok = repl_server_test_socketpair(&left, &right);
    char[512] response_buf = {};
    defer {
        if (left >= 0) c_close_fd(left);
        if (right >= 0) c_close_fd(right);
    }

    if (ok) {
        repl_server_reject_client_start_failure(left, true);
        ReplServerReadResult read = repl_server_read_line_fd(right, response_buf[..], true);
        ok = !read.failed &&
             !read.truncated &&
             read.len > 0 &&
             scheduler_test_str_contains(response_buf[:read.len], "runtime/client-handler-start-failed") &&
             scheduler_test_str_contains(response_buf[:read.len], "failed to start repl client handler");
        if (!ok) {
            io::printfn(
                "[FAIL] async repl-server client start failure rejects protocol error (failed=%s truncated=%s len=%d response=%s)",
                read.failed ? "yes" : "no",
                read.truncated ? "yes" : "no",
                (long)read.len,
                response_buf[:read.len]
            );
        }
    } else {
        io::printn("[FAIL] async repl-server client start failure rejects protocol error (socketpair)");
    }

    if (ok) {
        emit_pass_literal("[PASS] async repl-server client start failure rejects protocol error");
        (*pass)++;
    } else {
        (*fail)++;
    }
}

fn void run_async_tests_repl_server(Interp* interp, int* pass, int* fail) {
    run_async_repl_server_worker_start_requires_ready_primitives_test(interp, pass, fail);
    run_async_repl_server_worker_destroy_releases_sync_primitives_test(interp, pass, fail);
    run_async_repl_server_pending_interrupt_window_test(interp, pass, fail);
    run_async_repl_server_active_interrupt_test(interp, pass, fail);
    run_async_repl_server_request_queue_accepts_multiple_pending_test(interp, pass, fail);
    run_async_repl_server_request_queue_backpressure_test(interp, pass, fail);
    run_async_repl_server_clone_queue_defers_session_allocation_test(interp, pass, fail);
    run_async_repl_server_input_destroy_close_test(interp, pass, fail);
    run_async_repl_server_auth_policy_test(interp, pass, fail);
    run_async_repl_server_client_threads_handle_independent_streams_test(interp, pass, fail);
    run_async_repl_server_client_start_failure_rejects_protocol_error_test(interp, pass, fail);
}
