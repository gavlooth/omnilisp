module lisp;

fn bool repl_server_request_auth_exempt(char[] op) @inline {
    return json_repl_str_eq(op, "describe");
}

fn bool repl_server_connection_auth_matches(ReplServerConnection* conn, char[] auth_token) {
    if (conn == null || !conn.auth_required) return true;
    if (conn.auth_token_ptr == null || conn.auth_token_len == 0) return false;
    if (auth_token.len != conn.auth_token_len) return false;
    for (usz i = 0; i < conn.auth_token_len; i++) {
        if (conn.auth_token_ptr[i] != auth_token[i]) return false;
    }
    return true;
}

fn bool repl_server_request_is_authorized(ReplServerConnection* conn, ReplServerRequest* request) {
    if (conn == null || request == null) return false;
    if (!conn.auth_required || repl_server_request_auth_exempt(request.op)) return true;
    if (!request.has_auth) return false;
    return repl_server_connection_auth_matches(conn, request.auth);
}

fn bool repl_server_handle_request(ReplServerConnection* conn, ReplServerRequest* request, char[] transport_name) {
    ReplServerOutput* output = &conn.output;
    ReplServerWorker* worker = &conn.worker;
    char[] id = request.id;

    if (json_repl_str_eq(request.op, "describe")) {
        if (!repl_server_send_describe(output, id, transport_name, conn.auth_required)) return false;
        return repl_server_send_done(output, id, "");
    }

    if (!repl_server_request_is_authorized(conn, request)) {
        if (!request.has_auth) {
            return repl_server_send_protocol_error(
                output,
                id,
                "security/auth-required",
                "request field 'auth' is required for this transport"
            );
        }
        return repl_server_send_protocol_error(
            output,
            id,
            "security/auth-failed",
            "request field 'auth' does not match server policy"
        );
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
