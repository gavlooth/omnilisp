module lisp;

import std::core::mem;
import main;

fn bool repl_server_worker_busy_locked(ReplServerWorker* worker) @inline {
    return worker.command_pending || worker.busy;
}

fn bool repl_server_worker_busy(ReplServerWorker* worker) {
    if (worker == null || !worker.mu_ready) return false;
    worker.mu.lock();
    bool busy = repl_server_worker_busy_locked(worker);
    worker.mu.unlock();
    return busy;
}

fn bool repl_server_worker_command_matches(
    ReplServerCommand* command,
    ReplServerSession* session,
    char[] target_id
) @inline {
    if (command == null || command.session != session) return false;
    if (command.kind != REPL_SERVER_CMD_EVAL && command.kind != REPL_SERVER_CMD_LOAD_FILE) return false;
    if (command.id_len != target_id.len) return false;
    for (usz i = 0; i < target_id.len; i++) {
        if (command.id_ptr[i] != target_id[i]) return false;
    }
    return true;
}

fn bool repl_server_worker_matches_active_locked(
    ReplServerWorker* worker,
    ReplServerSession* session,
    char[] target_id
) @inline {
    if (worker == null || worker.active_session != session) return false;
    if (worker.active_kind != REPL_SERVER_CMD_EVAL && worker.active_kind != REPL_SERVER_CMD_LOAD_FILE) return false;
    if (worker.active_request_id_len != target_id.len) return false;
    for (usz i = 0; i < target_id.len; i++) {
        if (worker.active_request_id_ptr[i] != target_id[i]) return false;
    }
    return true;
}

fn bool repl_server_worker_submit(
    ReplServerWorker* worker,
    ReplServerCommandKind kind,
    ReplServerSession* session,
    char[] request_id,
    char[] payload = "",
    bool program_mode = false
) {
    if (worker == null || !worker.mu_ready || !worker.cv_ready) return false;

    char* id_copy = null;
    usz id_len = 0;
    if (!repl_server_dup_heap_string(request_id, &id_copy, &id_len)) return false;

    char* payload_copy = null;
    usz payload_len = 0;
    if (payload.len > 0 && !repl_server_dup_heap_string(payload, &payload_copy, &payload_len)) {
        mem::free(id_copy);
        return false;
    }

    worker.mu.lock();
    if (worker.stop_requested || repl_server_worker_busy_locked(worker)) {
        worker.mu.unlock();
        mem::free(id_copy);
        if (payload_copy != null) mem::free(payload_copy);
        return false;
    }

    worker.pending_command = {
        .kind = kind,
        .session = session,
        .program_mode = program_mode,
        .id_ptr = id_copy,
        .id_len = id_len,
        .payload_ptr = payload_copy,
        .payload_len = payload_len,
    };
    worker.command_pending = true;
    (void)worker.cv.signal();
    worker.mu.unlock();
    return true;
}

fn bool repl_server_worker_request_interrupt(
    ReplServerWorker* worker,
    ReplServerSession* session,
    char[] target_id
) {
    if (worker == null || !worker.mu_ready) return false;
    worker.mu.lock();
    bool matched = repl_server_worker_matches_active_locked(worker, session, target_id);
    if (matched) {
        session.interrupt_requested.store(1);
        io_input_state_wake(&session.input);
    }
    worker.mu.unlock();
    return matched;
}

fn bool repl_server_worker_accepts_stdin(
    ReplServerWorker* worker,
    ReplServerSession* session,
    char[] target_id
) {
    if (worker == null || !worker.mu_ready) return false;
    worker.mu.lock();
    bool matched = repl_server_worker_matches_active_locked(worker, session, target_id) ||
        repl_server_worker_command_matches(&worker.pending_command, session, target_id);
    worker.mu.unlock();
    return matched;
}
