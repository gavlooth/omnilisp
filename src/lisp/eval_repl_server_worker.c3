module lisp;

import std::core::mem;
import std::io;
import main;

fn void repl_server_worker_run_command(ReplServerWorker* worker, ReplServerCommand* command) {
    if (worker == null || command == null) return;
    ReplServerOutput* output = worker.output;
    ReplServerSession* session = command.session;
    if (session != null) session.interrupt_requested.store(0);

    switch (command.kind) {
        case REPL_SERVER_CMD_CLONE: {
            if (session == null && worker.conn != null) {
                session = repl_server_new_session(worker.conn);
            }
            if (session == null) {
                repl_server_send_protocol_error(output, command.id_ptr[:command.id_len], "runtime/session-create-failed", "failed to create session");
                return;
            }
            Interp* interp = null;
            if (!repl_server_create_interp(&interp)) {
                repl_server_send_protocol_error(output, command.id_ptr[:command.id_len], "runtime/session-create-failed", "failed to create session");
                repl_server_close_session(session);
                return;
            }
            interp.input_state = &session.input;
            interp.interrupt_flag = &session.interrupt_requested;
            session.interp = interp;
            session.ready = true;
            char[] session_id = session.id_ptr[:session.id_len];
            if (worker.conn != null && worker.conn.preload_project) {
                if (!repl_server_preload_project_session(output, session, command.id_ptr[:command.id_len], worker.conn.preload_project_dir)) {
                    repl_server_close_session(session);
                    return;
                }
            }
            if (!repl_server_send_session_event(output, command.id_ptr[:command.id_len], session_id)) {
                repl_server_close_session(session);
                return;
            }
            (void)repl_server_send_done(output, command.id_ptr[:command.id_len], session_id);
            return;
        }
        case REPL_SERVER_CMD_EVAL:
            if (session == null || !session.ready || session.interp == null) {
                (void)repl_server_send_protocol_error(output, command.id_ptr[:command.id_len], "protocol/unknown-session", "unknown session");
                return;
            }
            (void)repl_server_eval_session(output, session, command.id_ptr[:command.id_len], command.payload_ptr[:command.payload_len], command.program_mode);
            return;
        case REPL_SERVER_CMD_LOAD_FILE:
            if (session == null || !session.ready || session.interp == null) {
                (void)repl_server_send_protocol_error(output, command.id_ptr[:command.id_len], "protocol/unknown-session", "unknown session");
                return;
            }
            (void)repl_server_load_file_session(output, session, command.id_ptr[:command.id_len], command.payload_ptr[:command.payload_len]);
            return;
        case REPL_SERVER_CMD_COMPLETE:
            if (session == null || !session.ready || session.interp == null) {
                (void)repl_server_send_protocol_error(output, command.id_ptr[:command.id_len], "protocol/unknown-session", "unknown session");
                return;
            }
            (void)repl_server_complete_session(output, session, command.id_ptr[:command.id_len], command.payload_ptr[:command.payload_len]);
            return;
        case REPL_SERVER_CMD_CLOSE:
            if (session == null || !session.active) {
                (void)repl_server_send_protocol_error(output, command.id_ptr[:command.id_len], "protocol/unknown-session", "unknown session");
                return;
            }
            char[] session_id = session.id_ptr[:session.id_len];
            bool ok = repl_server_send_done(output, command.id_ptr[:command.id_len], session_id);
            repl_server_close_session(session);
            (void)ok;
            return;
        default:
            return;
    }
}

fn void repl_server_worker_thread_entry(void* ctx_ptr) {
    ReplServerWorker* worker = (ReplServerWorker*)ctx_ptr;
    if (worker == null) return;

    @pool_init(mem, 65536)
    {
        main::thread_registry_init();

        while (true) {
            worker.mu.lock();
            while (worker.command_queue_count == 0 && !worker.stop_requested) {
                (void)worker.cv.wait(&worker.mu);
            }
            if (worker.stop_requested && worker.command_queue_count == 0) {
                worker.mu.unlock();
                break;
            }

            ReplServerCommand command = worker.command_queue[worker.command_queue_head];
            worker.command_queue[worker.command_queue_head] = {};
            worker.command_queue_head = (worker.command_queue_head + 1) %
                REPL_SERVER_WORKER_QUEUE_CAPACITY;
            worker.command_queue_count--;
            worker.command_pending = worker.command_queue_count > 0;
            worker.busy = true;
            worker.active_kind = command.kind;
            worker.active_session = command.session;
            worker.active_request_id_ptr = command.id_ptr;
            worker.active_request_id_len = command.id_len;
            worker.mu.unlock();

            repl_server_worker_run_command(worker, &command);

            worker.mu.lock();
            if (worker.active_session != null) worker.active_session.interrupt_requested.store(0);
            worker.busy = false;
            worker.active_kind = REPL_SERVER_CMD_NONE;
            worker.active_session = null;
            worker.active_request_id_ptr = null;
            worker.active_request_id_len = 0;
            worker.mu.unlock();

            repl_server_command_free(&command);
        }

        if (worker.conn != null && worker.conn.sessions != null) {
            for (usz i = 0; i < worker.conn.session_capacity; i++) {
                repl_server_close_session(&worker.conn.sessions[i]);
            }
        }
    };
}

fn bool repl_server_worker_start(ReplServerWorker* worker) {
    if (worker == null) return false;
    if (!worker.mu_ready || !worker.cv_ready) return false;
    void* thread_handle = omni_uv_thread_new();
    if (thread_handle == null) return false;
    if (omni_uv_thread_start(thread_handle, &repl_server_worker_thread_entry, worker) != 0) {
        omni_uv_thread_free(thread_handle);
        return false;
    }
    worker.thread_handle = thread_handle;
    worker.thread_started = true;
    return true;
}

fn void repl_server_worker_shutdown(ReplServerWorker* worker) {
    if (worker == null || !worker.thread_started || worker.thread_handle == null || !worker.mu_ready) return;
    worker.mu.lock();
    worker.stop_requested = true;
    if (worker.cv_ready) (void)worker.cv.broadcast();
    worker.mu.unlock();
    (void)omni_uv_thread_join_handle(worker.thread_handle);
    omni_uv_thread_free(worker.thread_handle);
    worker.thread_handle = null;
    worker.thread_started = false;
}

fn void repl_server_worker_destroy(ReplServerWorker* worker) {
    if (worker == null) return;
    repl_server_worker_shutdown(worker);
    repl_server_worker_clear_queued_commands(worker);
    if (worker.cv_ready) {
        (void)worker.cv.destroy();
        worker.cv_ready = false;
    }
    if (worker.mu_ready) {
        (void)worker.mu.destroy();
        worker.mu_ready = false;
    }
    worker.conn = null;
    worker.output = null;
}
