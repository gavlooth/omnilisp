module lisp;

import std::core::mem;
import std::io;
import std::thread;
import std::atomic::types;
import main;

struct ReplServerSession {
    bool active;
    bool ready;
    char* id_ptr;
    usz id_len;
    Interp* interp;
    InterpInputState input;
    types::Atomic{int} interrupt_requested;
}

enum ReplServerCommandKind : char {
    REPL_SERVER_CMD_NONE,
    REPL_SERVER_CMD_CLONE,
    REPL_SERVER_CMD_EVAL,
    REPL_SERVER_CMD_LOAD_FILE,
    REPL_SERVER_CMD_COMPLETE,
    REPL_SERVER_CMD_CLOSE
}

struct ReplServerCommand {
    ReplServerCommandKind kind;
    ReplServerSession* session;
    bool program_mode;
    char* id_ptr;
    usz id_len;
    char* payload_ptr;
    usz payload_len;
}

struct ReplServerWorker {
    ReplServerConnection* conn;
    ReplServerOutput* output;
    thread::Mutex mu;
    thread::ConditionVariable cv;
    bool mu_ready;
    bool cv_ready;
    bool stop_requested;
    bool command_pending;
    bool busy;
    ReplServerCommand pending_command;
    ReplServerCommandKind active_kind;
    ReplServerSession* active_session;
    char* active_request_id_ptr;
    usz active_request_id_len;
    void* thread_handle;
    bool thread_started;
}

struct ReplServerConnection {
    ReplServerSession* sessions;
    usz session_capacity;
    usz next_session_id;
    bool auth_required;
    char* auth_token_ptr;
    usz auth_token_len;
    bool preload_project;
    char* preload_project_dir;
    ReplServerOutput output;
    ReplServerWorker worker;
}

fn usz repl_server_cstr_len(char* text) @inline {
    if (text == null) return 0;
    usz len = 0;
    while (text[len] != 0) len++;
    return len;
}


fn bool repl_server_create_interp(Interp** out_interp) {
    *out_interp = null;
    Interp* interp = main::repl_create_plain_interp();
    if (interp == null) return false;
    if (!bootstrap_runtime_interp(interp)) {
        main::repl_destroy_plain_interp(interp);
        return false;
    }
    interp.flags.jit_enabled = true;
    *out_interp = interp;
    return true;
}

fn void repl_server_destroy_interp(Interp* interp) {
    if (interp == null) return;
    main::repl_destroy_plain_interp(interp);
}

fn void repl_server_close_session(ReplServerSession* session) {
    if (session == null || !session.active) return;
    session.interrupt_requested.store(0);
    (void)io_input_state_close(&session.input);
    if (session.id_ptr != null) mem::free(session.id_ptr);
    if (session.interp != null) repl_server_destroy_interp(session.interp);
    io_input_state_destroy(&session.input);
    *session = {};
}

fn bool repl_server_dup_heap_string(char[] text, char** out_ptr, usz* out_len) {
    *out_ptr = null;
    *out_len = 0;
    if (text.len == usz.max) return false;
    char* copy = (char*)mem::malloc(text.len + 1);
    if (copy == null) return false;
    if (text.len > 0) mem::copy(copy, text.ptr, text.len);
    copy[text.len] = 0;
    *out_ptr = copy;
    *out_len = text.len;
    return true;
}

fn void repl_server_command_free(ReplServerCommand* command) {
    if (command == null) return;
    if (command.id_ptr != null) mem::free(command.id_ptr);
    if (command.payload_ptr != null) mem::free(command.payload_ptr);
    *command = {};
}

fn void repl_server_worker_init(ReplServerWorker* worker, ReplServerConnection* conn, ReplServerOutput* output) {
    *worker = {};
    worker.conn = conn;
    worker.output = output;
    if (catch err = worker.mu.init()) {
        worker.mu_ready = false;
    } else {
        worker.mu_ready = true;
    }
    if (catch err = worker.cv.init()) {
        worker.cv_ready = false;
    } else {
        worker.cv_ready = true;
    }
}

fn void repl_server_connection_init(
    ReplServerConnection* conn,
    int write_fd,
    bool socket_write,
    bool preload_project = false,
    char* preload_project_dir = null,
    bool auth_required = false,
    char* auth_token_ptr = null
) {
    *conn = {};
    conn.next_session_id = 1;
    conn.auth_required = auth_required && auth_token_ptr != null && auth_token_ptr[0] != 0;
    conn.auth_token_ptr = conn.auth_required ? auth_token_ptr : null;
    conn.auth_token_len = conn.auth_required ? repl_server_cstr_len(auth_token_ptr) : 0;
    conn.preload_project = preload_project;
    conn.preload_project_dir = preload_project_dir;
    repl_server_output_init(&conn.output, write_fd, socket_write);
    repl_server_worker_init(&conn.worker, conn, &conn.output);
}

fn void repl_server_connection_destroy(ReplServerConnection* conn) {
    if (conn.sessions != null) {
        for (usz i = 0; i < conn.session_capacity; i++) {
            if (conn.sessions[i].active) repl_server_close_session(&conn.sessions[i]);
        }
        mem::free(conn.sessions);
    }
    repl_server_command_free(&conn.worker.pending_command);
    repl_server_output_destroy(&conn.output);
    *conn = {};
}

fn bool repl_server_ensure_session_capacity(ReplServerConnection* conn, usz needed) {
    if (needed <= conn.session_capacity) return true;
    if (conn.session_capacity > usz.max / (usz)2) return false;
    usz next_cap = conn.session_capacity > 0 ? conn.session_capacity * 2 : 4;
    while (next_cap < needed) {
        if (next_cap > usz.max / (usz)2) return false;
        next_cap *= 2;
    }
    if (next_cap > usz.max / ReplServerSession.sizeof) return false;
    ReplServerSession* next = conn.sessions == null
        ? (ReplServerSession*)mem::malloc(ReplServerSession.sizeof * next_cap)
        : (ReplServerSession*)mem::realloc(conn.sessions, ReplServerSession.sizeof * next_cap);
    if (next == null) return false;
    for (usz i = conn.session_capacity; i < next_cap; i++) next[i] = {};
    conn.sessions = next;
    conn.session_capacity = next_cap;
    return true;
}

fn ReplServerSession* repl_server_find_session(ReplServerConnection* conn, char[] session_id) {
    for (usz i = 0; i < conn.session_capacity; i++) {
        if (!conn.sessions[i].active) continue;
        if (conn.sessions[i].id_len != session_id.len) continue;
        bool match = true;
        for (usz j = 0; j < session_id.len; j++) {
            if (conn.sessions[i].id_ptr[j] != session_id[j]) {
                match = false;
                break;
            }
        }
        if (match) return &conn.sessions[i];
    }
    return null;
}

fn ReplServerSession* repl_server_new_session(ReplServerConnection* conn) {
    usz old_capacity = conn.session_capacity;
    usz slot = conn.session_capacity;
    for (usz i = 0; i < conn.session_capacity; i++) {
        if (!conn.sessions[i].active) {
            slot = i;
            break;
        }
    }
    if (slot == conn.session_capacity && conn.session_capacity == usz.max) return null;
    if (slot == conn.session_capacity && !repl_server_ensure_session_capacity(conn, conn.session_capacity + 1)) {
        return null;
    }
    if (slot == old_capacity) slot = old_capacity;

    char[REPL_SERVER_SESSION_ID_MAX] id_buf = {};
    char[] id;
    if (conn.next_session_id > (usz)long.max) return null;
    if (try rendered = io::bprintf(&id_buf, "s%d", (long)conn.next_session_id)) {
        id = rendered;
    } else {
        return null;
    }

    char* stored_id = null;
    usz stored_id_len = 0;
    if (!repl_server_dup_heap_string(id, &stored_id, &stored_id_len)) return null;
    conn.next_session_id++;

    conn.sessions[slot] = {
        .active = true,
        .ready = false,
        .id_ptr = stored_id,
        .id_len = stored_id_len,
        .interp = null,
    };
    io_input_state_init(&conn.sessions[slot].input);
    if (!conn.sessions[slot].input.mu_ready || !conn.sessions[slot].input.cv_ready) {
        repl_server_close_session(&conn.sessions[slot]);
        return null;
    }
    conn.sessions[slot].interrupt_requested.store(0);
    return &conn.sessions[slot];
}
