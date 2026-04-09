module lisp;

import std::io;

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
                           read.status == IO_INPUT_READ_EOF &&
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

fn void run_async_tests_repl_server(Interp* interp, int* pass, int* fail) {
    run_async_repl_server_pending_interrupt_window_test(interp, pass, fail);
    run_async_repl_server_active_interrupt_test(interp, pass, fail);
    run_async_repl_server_input_destroy_close_test(interp, pass, fail);
    run_async_repl_server_auth_policy_test(interp, pass, fail);
}
