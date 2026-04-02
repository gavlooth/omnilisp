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

fn void run_async_tests_repl_server(Interp* interp, int* pass, int* fail) {
    run_async_repl_server_pending_interrupt_window_test(interp, pass, fail);
    run_async_repl_server_active_interrupt_test(interp, pass, fail);
}
