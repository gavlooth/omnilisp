module lisp;

import std::core::mem;
import main;

fn bool repl_server_worker_busy_locked(ReplServerWorker* worker) @inline {
    return worker.busy || worker.command_queue_count >= REPL_SERVER_WORKER_QUEUE_CAPACITY;
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

    usz queue_idx = (worker.command_queue_head + worker.command_queue_count) %
        REPL_SERVER_WORKER_QUEUE_CAPACITY;
    worker.command_queue[queue_idx] = {
        .kind = kind,
        .session = session,
        .program_mode = program_mode,
        .id_ptr = id_copy,
        .id_len = id_len,
        .payload_ptr = payload_copy,
        .payload_len = payload_len,
    };
    worker.command_queue_count++;
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
    bool matched = repl_server_worker_matches_active_locked(worker, session, target_id);
    if (!matched) {
        for (usz i = 0; i < worker.command_queue_count; i++) {
            usz queue_idx = (worker.command_queue_head + i) %
                REPL_SERVER_WORKER_QUEUE_CAPACITY;
            if (repl_server_worker_command_matches(&worker.command_queue[queue_idx], session, target_id)) {
                matched = true;
                break;
            }
        }
    }
    worker.mu.unlock();
    return matched;
}

fn void repl_server_worker_clear_queued_commands(ReplServerWorker* worker) {
    if (worker == null) return;
    bool locked = worker.mu_ready;
    if (locked) worker.mu.lock();
    defer if (locked) worker.mu.unlock();
    for (usz i = 0; i < worker.command_queue_count; i++) {
        usz queue_idx = (worker.command_queue_head + i) %
            REPL_SERVER_WORKER_QUEUE_CAPACITY;
        repl_server_command_free(&worker.command_queue[queue_idx]);
    }
    worker.command_queue_head = 0;
    worker.command_queue_count = 0;
    worker.command_pending = false;
    repl_server_command_free(&worker.pending_command);
}
