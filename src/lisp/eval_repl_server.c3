module lisp;

import std::core::mem;
import std::io;
import std::thread;
import std::atomic::types;
import main;

extern fn int omni_unix_socket_listen_fd(char* path, int backlog, int* out_fd) @extern("omni_unix_socket_listen_fd");
const usz REPL_SERVER_LINE_MAX = 65536;
const usz REPL_SERVER_SESSION_ID_MAX = 32;
const int REPL_SERVER_LISTEN_BACKLOG = 16;
const int REPL_SERVER_SEND_FLAGS = 0x4000; // MSG_NOSIGNAL on Linux

struct ReplServerReadResult {
    usz len;
    bool eof;
    bool truncated;
    bool failed;
}

struct ReplServerRequest {
    bool ok;
    bool has_id;
    bool has_op;
    bool has_session;
    bool has_target;
    bool has_code;
    bool has_path;
    bool has_prefix;
    bool has_data;
    bool has_eof;
    bool eof;
    bool program_mode;
    char[] id;
    char[] op;
    char[] session;
    char[] target;
    char[] code;
    char[] path;
    char[] prefix;
    char[] data;
    char[] error_message;
}

struct ReplServerSession {
    bool active;
    bool ready;
    char* id_ptr;
    usz id_len;
    Interp* interp;
    InterpInputState input;
    types::Atomic{int} interrupt_requested;
}

struct ReplServerOutput {
    int write_fd;
    bool socket_write;
    thread::Mutex write_mu;
    bool write_mu_ready;
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
    bool preload_project;
    char* preload_project_dir;
    ReplServerOutput output;
    ReplServerWorker worker;
}

fn bool repl_server_write_all_raw(int fd, char[] text, bool socket_write) {
    usz offset = 0;
    while (offset < text.len) {
        long wrote_n = socket_write
            ? c_send(fd, text.ptr + offset, text.len - offset, REPL_SERVER_SEND_FLAGS)
            : omni_uv_fs_write(fd, text.ptr + offset, text.len - offset);
        if (wrote_n <= 0) return false;
        offset += (usz)wrote_n;
    }
    return true;
}

fn bool repl_server_write_text_file_atomic(char[] final_path, char[] text) {
    char[256] temp_path_buf;
    char[] temp_path = "";
    if (catch err = io::bprintf(&temp_path_buf, "%s.tmp.%d", (ZString)final_path, c_getpid())) {
        return false;
    } else {
        temp_path = io::bprintf(&temp_path_buf, "%s.tmp.%d", (ZString)final_path, c_getpid())!!;
    }

    (void)omni_uv_fs_unlink(&temp_path_buf[0]);

    bool write_ok = false;
    if (try file = io::file::open((String)temp_path, "w")) {
        defer if (!write_ok) (void)omni_uv_fs_unlink(&temp_path_buf[0]);

        char[] remaining = text;
        write_ok = true;
        while (write_ok && remaining.len > 0) {
            usz? written_opt = file.write(remaining);
            if (catch err = written_opt) {
                write_ok = false;
                break;
            }
            if (written_opt == 0) {
                write_ok = false;
                break;
            }
            remaining = remaining[written_opt..];
        }
        if (catch err = file.close()) write_ok = false;
        if (!write_ok) return false;
        if (omni_uv_fs_rename(&temp_path_buf[0], (ZString)final_path.ptr) != 0) {
            write_ok = false;
            return false;
        }
        return true;
    }
    return false;
}

fn bool repl_server_publish_tcp_port_file(int port) {
    char[32] text_buf;
    char[] text = "";
    if (catch err = io::bprintf(&text_buf, "%d\n", port)) {
        return false;
    } else {
        text = io::bprintf(&text_buf, "%d\n", port)!!;
    }
    return repl_server_write_text_file_atomic(".omni-repl-port", text);
}

fn bool repl_server_output_write_json_doc(ReplServerOutput* output, YyjsonMutDoc* doc) {
    if (output == null) return false;
    usz len = 0;
    char* json = omni_yyjson_mut_write_with_flags(doc, 0, &len);
    if (json == null) return false;
    defer c_free_json(json);
    if (output.write_mu_ready) output.write_mu.lock();
    defer if (output.write_mu_ready) output.write_mu.unlock();
    if (!repl_server_write_all_raw(output.write_fd, json[:len], output.socket_write)) return false;
    char[1] nl = {'\n'};
    return repl_server_write_all_raw(output.write_fd, nl[..], output.socket_write);
}

fn void repl_server_obj_add_string(YyjsonMutDoc* doc, YyjsonMutVal* obj, char[] key, char[] value) {
    omni_yyjson_mut_obj_add(
        obj,
        omni_yyjson_mut_strn(doc, key.ptr, key.len),
        omni_yyjson_mut_strn(doc, value.ptr, value.len)
    );
}

fn void repl_server_obj_add_bool(YyjsonMutDoc* doc, YyjsonMutVal* obj, char[] key, bool value) {
    omni_yyjson_mut_obj_add(
        obj,
        omni_yyjson_mut_strn(doc, key.ptr, key.len),
        omni_yyjson_mut_bool(doc, value ? 1 : 0)
    );
}

fn void repl_server_obj_add_sint(YyjsonMutDoc* doc, YyjsonMutVal* obj, char[] key, long value) {
    omni_yyjson_mut_obj_add(
        obj,
        omni_yyjson_mut_strn(doc, key.ptr, key.len),
        omni_yyjson_mut_sint(doc, value)
    );
}

fn void repl_server_obj_add_null(YyjsonMutDoc* doc, YyjsonMutVal* obj, char[] key) {
    omni_yyjson_mut_obj_add(
        obj,
        omni_yyjson_mut_strn(doc, key.ptr, key.len),
        omni_yyjson_mut_null(doc)
    );
}

fn void repl_server_obj_add_val(YyjsonMutDoc* doc, YyjsonMutVal* obj, char[] key, YyjsonMutVal* value) {
    omni_yyjson_mut_obj_add(
        obj,
        omni_yyjson_mut_strn(doc, key.ptr, key.len),
        value
    );
}

fn bool repl_server_send_event_error(
    ReplServerOutput* output,
    char[] id,
    char[] session,
    char[] code,
    char[] message,
    bool include_range = false,
    usz line = 0,
    usz column = 0
) {
    YyjsonMutDoc* doc = omni_yyjson_mut_doc_new();
    if (doc == null) return false;
    defer omni_yyjson_mut_doc_free(doc);

    YyjsonMutVal* root = omni_yyjson_mut_obj(doc);
    omni_yyjson_mut_doc_set_root(doc, root);
    repl_server_obj_add_string(doc, root, "id", id);
    if (session.len > 0) repl_server_obj_add_string(doc, root, "session", session);
    repl_server_obj_add_string(doc, root, "event", "error");

    YyjsonMutVal* err = omni_yyjson_mut_obj(doc);
    repl_server_obj_add_string(doc, err, "code", code);
    repl_server_obj_add_string(doc, err, "message", message);

    if (include_range) {
        YyjsonMutVal* range = omni_yyjson_mut_obj(doc);
        YyjsonMutVal* start = omni_yyjson_mut_obj(doc);
        YyjsonMutVal* end = omni_yyjson_mut_obj(doc);
        repl_server_obj_add_sint(doc, start, "line", (long)line);
        repl_server_obj_add_sint(doc, start, "character", (long)column);
        repl_server_obj_add_sint(doc, end, "line", (long)line);
        repl_server_obj_add_sint(doc, end, "character", (long)(column + 1));
        repl_server_obj_add_val(doc, range, "start", start);
        repl_server_obj_add_val(doc, range, "end", end);
        repl_server_obj_add_val(doc, err, "range", range);
    }

    repl_server_obj_add_val(doc, root, "error", err);
    return repl_server_output_write_json_doc(output, doc);
}

fn bool repl_server_send_protocol_error(ReplServerOutput* output, char[] id, char[] code, char[] message) {
    return repl_server_send_event_error(output, id, "", code, message);
}

fn bool repl_server_send_simple_event(ReplServerOutput* output, char[] id, char[] session_id, char[] event_name) {
    YyjsonMutDoc* doc = omni_yyjson_mut_doc_new();
    if (doc == null) return false;
    defer omni_yyjson_mut_doc_free(doc);

    YyjsonMutVal* root = omni_yyjson_mut_obj(doc);
    omni_yyjson_mut_doc_set_root(doc, root);
    repl_server_obj_add_string(doc, root, "id", id);
    if (session_id.len > 0) repl_server_obj_add_string(doc, root, "session", session_id);
    repl_server_obj_add_string(doc, root, "event", event_name);
    return repl_server_output_write_json_doc(output, doc);
}

fn bool repl_server_send_session_event(ReplServerOutput* output, char[] id, char[] session_id) {
    return repl_server_send_simple_event(output, id, session_id, "session");
}

fn bool repl_server_send_done(ReplServerOutput* output, char[] id, char[] session_id) {
    return repl_server_send_simple_event(output, id, session_id, "done");
}

fn bool repl_server_send_out_event(ReplServerOutput* output, char[] id, char[] session, char[] text) {
    YyjsonMutDoc* doc = omni_yyjson_mut_doc_new();
    if (doc == null) return false;
    defer omni_yyjson_mut_doc_free(doc);

    YyjsonMutVal* root = omni_yyjson_mut_obj(doc);
    omni_yyjson_mut_doc_set_root(doc, root);
    repl_server_obj_add_string(doc, root, "id", id);
    repl_server_obj_add_string(doc, root, "session", session);
    repl_server_obj_add_string(doc, root, "event", "out");
    repl_server_obj_add_string(doc, root, "text", text);
    return repl_server_output_write_json_doc(output, doc);
}

fn bool repl_server_send_value_event(ReplServerOutput* output, char[] id, char[] session, char[] value) {
    YyjsonMutDoc* doc = omni_yyjson_mut_doc_new();
    if (doc == null) return false;
    defer omni_yyjson_mut_doc_free(doc);

    YyjsonMutVal* root = omni_yyjson_mut_obj(doc);
    omni_yyjson_mut_doc_set_root(doc, root);
    repl_server_obj_add_string(doc, root, "id", id);
    repl_server_obj_add_string(doc, root, "session", session);
    repl_server_obj_add_string(doc, root, "event", "value");
    repl_server_obj_add_string(doc, root, "value", value);
    return repl_server_output_write_json_doc(output, doc);
}

fn bool repl_server_send_loaded_event(ReplServerOutput* output, char[] id, char[] session, char[] path) {
    YyjsonMutDoc* doc = omni_yyjson_mut_doc_new();
    if (doc == null) return false;
    defer omni_yyjson_mut_doc_free(doc);

    YyjsonMutVal* root = omni_yyjson_mut_obj(doc);
    omni_yyjson_mut_doc_set_root(doc, root);
    repl_server_obj_add_string(doc, root, "id", id);
    repl_server_obj_add_string(doc, root, "session", session);
    repl_server_obj_add_string(doc, root, "event", "loaded");
    repl_server_obj_add_string(doc, root, "path", path);
    return repl_server_output_write_json_doc(output, doc);
}

fn char[] repl_server_completion_kind(Value* value) {
    if (value == null) return "binding";
    switch (value.tag) {
        case PRIMITIVE:
        case CLOSURE:
        case PARTIAL_PRIM:
        case METHOD_TABLE:
            return "function";
        case MODULE:
            return "module";
        default:
            return "binding";
    }
}

fn bool repl_server_completion_name_less(char[] left, char[] right) {
    usz limit = left.len < right.len ? left.len : right.len;
    for (usz i = 0; i < limit; i++) {
        if (left[i] < right[i]) return true;
        if (left[i] > right[i]) return false;
    }
    return left.len < right.len;
}

fn bool repl_server_send_interrupted_event(ReplServerOutput* output, char[] id, char[] session) {
    return repl_server_send_simple_event(output, id, session, "interrupted");
}

fn bool repl_server_send_complete_event(ReplServerOutput* output, char[] id, char[] session, Env* env, SymbolTable* symbols, char[] prefix) {
    YyjsonMutDoc* doc = omni_yyjson_mut_doc_new();
    if (doc == null) return false;
    defer omni_yyjson_mut_doc_free(doc);

    YyjsonMutVal* root = omni_yyjson_mut_obj(doc);
    omni_yyjson_mut_doc_set_root(doc, root);
    repl_server_obj_add_string(doc, root, "id", id);
    repl_server_obj_add_string(doc, root, "session", session);
    repl_server_obj_add_string(doc, root, "event", "complete");

    YyjsonMutVal* items = omni_yyjson_mut_arr(doc);
    if (env != null && env.binding_count > 0) {
        usz* matches = (usz*)mem::malloc(usz.sizeof * env.binding_count);
        if (matches == null) return false;
        defer mem::free(matches);

        usz match_count = 0;
        for (usz i = 0; i < env.binding_count; i++) {
            char[] name = symbols.get_name(env.bindings[i].name);
            if (!repl_completion_matches_prefix(name, prefix.ptr, prefix.len)) continue;
            matches[match_count] = i;
            match_count++;
        }

        for (usz i = 1; i < match_count; i++) {
            usz current = matches[i];
            char[] current_name = symbols.get_name(env.bindings[current].name);
            usz j = i;
            while (j > 0) {
                char[] prev_name = symbols.get_name(env.bindings[matches[j - 1]].name);
                if (!repl_server_completion_name_less(current_name, prev_name)) break;
                matches[j] = matches[j - 1];
                j--;
            }
            matches[j] = current;
        }

        for (usz i = 0; i < match_count; i++) {
            Binding binding = env.bindings[matches[i]];
            char[] name = symbols.get_name(binding.name);
            YyjsonMutVal* item = omni_yyjson_mut_obj(doc);
            repl_server_obj_add_string(doc, item, "candidate", name);
            repl_server_obj_add_string(doc, item, "kind", repl_server_completion_kind(binding.value));
            omni_yyjson_mut_arr_append(items, item);
        }
    }

    repl_server_obj_add_val(doc, root, "items", items);
    return repl_server_output_write_json_doc(output, doc);
}

fn bool repl_server_send_describe(ReplServerOutput* output, char[] id, char[] transport_name) {
    YyjsonMutDoc* doc = omni_yyjson_mut_doc_new();
    if (doc == null) return false;
    defer omni_yyjson_mut_doc_free(doc);

    YyjsonMutVal* root = omni_yyjson_mut_obj(doc);
    omni_yyjson_mut_doc_set_root(doc, root);
    repl_server_obj_add_string(doc, root, "id", id);
    repl_server_obj_add_string(doc, root, "event", "describe");
    repl_server_obj_add_string(doc, root, "protocol", "omni-repl/1");

    YyjsonMutVal* server = omni_yyjson_mut_obj(doc);
    repl_server_obj_add_string(doc, server, "implementation", "omni");
    repl_server_obj_add_string(doc, server, "version", "0.2.0");
    repl_server_obj_add_val(doc, root, "server", server);

    YyjsonMutVal* transports = omni_yyjson_mut_arr(doc);
    omni_yyjson_mut_arr_append(transports, omni_yyjson_mut_strn(doc, transport_name.ptr, transport_name.len));
    repl_server_obj_add_val(doc, root, "transports", transports);

    YyjsonMutVal* ops = omni_yyjson_mut_obj(doc);
    repl_server_obj_add_val(doc, ops, "clone", omni_yyjson_mut_obj(doc));
    repl_server_obj_add_val(doc, ops, "close", omni_yyjson_mut_obj(doc));
    repl_server_obj_add_val(doc, ops, "describe", omni_yyjson_mut_obj(doc));
    repl_server_obj_add_val(doc, ops, "complete", omni_yyjson_mut_obj(doc));
    repl_server_obj_add_val(doc, ops, "interrupt", omni_yyjson_mut_obj(doc));
    repl_server_obj_add_val(doc, ops, "stdin", omni_yyjson_mut_obj(doc));
    YyjsonMutVal* eval = omni_yyjson_mut_obj(doc);
    YyjsonMutVal* modes = omni_yyjson_mut_arr(doc);
    omni_yyjson_mut_arr_append(modes, omni_yyjson_mut_strn(doc, "expr".ptr, 4));
    omni_yyjson_mut_arr_append(modes, omni_yyjson_mut_strn(doc, "program".ptr, 7));
    repl_server_obj_add_val(doc, eval, "modes", modes);
    repl_server_obj_add_val(doc, ops, "eval", eval);
    repl_server_obj_add_val(doc, ops, "load-file", omni_yyjson_mut_obj(doc));
    repl_server_obj_add_val(doc, root, "ops", ops);

    return repl_server_output_write_json_doc(output, doc);
}

fn ReplServerReadResult repl_server_read_line_fd(int fd, char[] buffer, bool socket_read) {
    ReplServerReadResult out = { .len = 0, .eof = false, .truncated = false, .failed = false };
    usz max_len = buffer.len - 1;
    char ch = 0;

    while (out.len < max_len) {
        long nread = socket_read ? c_recv(fd, &ch, 1, 0) : omni_uv_fs_read(fd, &ch, 1);
        if (nread == 0) {
            out.eof = true;
            break;
        }
        if (nread < 0) {
            out.failed = true;
            break;
        }
        if (ch == '\n') break;
        buffer[out.len] = ch;
        out.len++;
    }

    if (!out.eof && !out.failed && out.len >= max_len) {
        while (true) {
            long nread = socket_read ? c_recv(fd, &ch, 1, 0) : omni_uv_fs_read(fd, &ch, 1);
            if (nread == 0) {
                out.eof = true;
                break;
            }
            if (nread < 0) {
                out.failed = true;
                break;
            }
            if (ch == '\n') break;
            out.truncated = true;
        }
    }

    buffer[out.len] = 0;
    return out;
}

fn void repl_server_request_free(ReplServerRequest* request) {
    if (request == null) return;
    if (request.has_id && request.id.ptr != null) mem::free(request.id.ptr);
    if (request.has_op && request.op.ptr != null) mem::free(request.op.ptr);
    if (request.has_session && request.session.ptr != null) mem::free(request.session.ptr);
    if (request.has_target && request.target.ptr != null) mem::free(request.target.ptr);
    if (request.has_code && request.code.ptr != null) mem::free(request.code.ptr);
    if (request.has_path && request.path.ptr != null) mem::free(request.path.ptr);
    if (request.has_prefix && request.prefix.ptr != null) mem::free(request.prefix.ptr);
    if (request.has_data && request.data.ptr != null) mem::free(request.data.ptr);
    *request = {};
}

fn ReplServerRequest repl_server_parse_request(char[] line) {
    ReplServerRequest request = {
        .ok = false,
        .error_message = "invalid request",
    };

    YyjsonDoc* doc = omni_yyjson_read(line.ptr, line.len, 0);
    if (doc == null) {
        request.error_message = "invalid JSON request";
        return request;
    }
    defer omni_yyjson_doc_free(doc);

    YyjsonVal* root = omni_yyjson_doc_get_root(doc);
    if (root == null || omni_yyjson_is_obj(root) == 0) {
        request.error_message = "request must be a JSON object";
        return request;
    }

    YyjsonVal* id_val = omni_yyjson_obj_getn(root, "id".ptr, 2);
    if (id_val == null) {
        request.error_message = "request field 'id' is required";
        return request;
    }
    char[] parsed_id = "";
    if (!json_repl_val_string(id_val, &parsed_id)) {
        request.error_message = "request field 'id' must be a string";
        return request;
    }
    request.id = json_repl_dup_string(parsed_id);
    if (request.id.ptr == null) {
        request.error_message = "out of memory while copying request field 'id'";
        return request;
    }
    request.has_id = true;

    YyjsonVal* op_val = omni_yyjson_obj_getn(root, "op".ptr, 2);
    if (op_val == null) {
        request.error_message = "request field 'op' is required";
        return request;
    }
    char[] parsed_op = "";
    if (!json_repl_val_string(op_val, &parsed_op)) {
        request.error_message = "request field 'op' must be a string";
        return request;
    }
    request.op = json_repl_dup_string(parsed_op);
    if (request.op.ptr == null) {
        request.error_message = "out of memory while copying request field 'op'";
        return request;
    }
    request.has_op = true;

    YyjsonVal* session_val = omni_yyjson_obj_getn(root, "session".ptr, 7);
    if (session_val != null) {
        char[] parsed_session = "";
        if (!json_repl_val_string(session_val, &parsed_session)) {
            request.error_message = "request field 'session' must be a string";
            return request;
        }
        request.session = json_repl_dup_string(parsed_session);
        if (request.session.ptr == null) {
            request.error_message = "out of memory while copying request field 'session'";
            return request;
        }
        request.has_session = true;
    }

    YyjsonVal* target_val = omni_yyjson_obj_getn(root, "target".ptr, 6);
    if (target_val != null) {
        char[] parsed_target = "";
        if (!json_repl_val_string(target_val, &parsed_target)) {
            request.error_message = "request field 'target' must be a string";
            return request;
        }
        request.target = json_repl_dup_string(parsed_target);
        if (request.target.ptr == null) {
            request.error_message = "out of memory while copying request field 'target'";
            return request;
        }
        request.has_target = true;
    }

    YyjsonVal* code_val = omni_yyjson_obj_getn(root, "code".ptr, 4);
    if (code_val != null) {
        char[] parsed_code = "";
        if (!json_repl_val_string(code_val, &parsed_code)) {
            request.error_message = "request field 'code' must be a string";
            return request;
        }
        request.code = json_repl_dup_string(parsed_code);
        if (request.code.ptr == null) {
            request.error_message = "out of memory while copying request field 'code'";
            return request;
        }
        request.has_code = true;
    }

    YyjsonVal* path_val = omni_yyjson_obj_getn(root, "path".ptr, 4);
    if (path_val != null) {
        char[] parsed_path = "";
        if (!json_repl_val_string(path_val, &parsed_path)) {
            request.error_message = "request field 'path' must be a string";
            return request;
        }
        request.path = json_repl_dup_string(parsed_path);
        if (request.path.ptr == null) {
            request.error_message = "out of memory while copying request field 'path'";
            return request;
        }
        request.has_path = true;
    }

    YyjsonVal* prefix_val = omni_yyjson_obj_getn(root, "prefix".ptr, 6);
    if (prefix_val != null) {
        char[] parsed_prefix = "";
        if (!json_repl_val_string(prefix_val, &parsed_prefix)) {
            request.error_message = "request field 'prefix' must be a string";
            return request;
        }
        request.prefix = json_repl_dup_string(parsed_prefix);
        if (request.prefix.ptr == null) {
            request.error_message = "out of memory while copying request field 'prefix'";
            return request;
        }
        request.has_prefix = true;
    }

    YyjsonVal* data_val = omni_yyjson_obj_getn(root, "data".ptr, 4);
    if (data_val != null) {
        char[] parsed_data = "";
        if (!json_repl_val_string(data_val, &parsed_data)) {
            request.error_message = "request field 'data' must be a string";
            return request;
        }
        request.data = json_repl_dup_string(parsed_data);
        if (request.data.ptr == null) {
            request.error_message = "out of memory while copying request field 'data'";
            return request;
        }
        request.has_data = true;
    }

    YyjsonVal* eof_val = omni_yyjson_obj_getn(root, "eof".ptr, 3);
    if (eof_val != null) {
        if (omni_yyjson_is_true(eof_val) == 0 && omni_yyjson_is_false(eof_val) == 0) {
            request.error_message = "request field 'eof' must be a boolean";
            return request;
        }
        request.eof = omni_yyjson_get_bool(eof_val) != 0;
        request.has_eof = true;
    }

    YyjsonVal* mode_val = omni_yyjson_obj_getn(root, "mode".ptr, 4);
    if (mode_val != null) {
        char[] mode = "";
        if (!json_repl_val_string(mode_val, &mode)) {
            request.error_message = "request field 'mode' must be a string";
            return request;
        }
        if (json_repl_str_eq(mode, "expr")) {
            request.program_mode = false;
        } else if (json_repl_str_eq(mode, "program")) {
            request.program_mode = true;
        } else {
            request.error_message = "request field 'mode' must be 'expr' or 'program'";
            return request;
        }
    }

    request.ok = true;
    return request;
}

fn bool repl_server_create_interp(Interp** out_interp) {
    *out_interp = null;
    Interp* interp = main::create_plain_interp();
    if (interp == null) return false;
    if (!bootstrap_runtime_interp(interp)) {
        main::destroy_plain_interp(interp);
        return false;
    }
    interp.flags.jit_enabled = true;
    *out_interp = interp;
    return true;
}

fn void repl_server_destroy_interp(Interp* interp) {
    if (interp == null) return;
    main::destroy_plain_interp(interp);
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

fn void repl_server_output_init(ReplServerOutput* output, int write_fd, bool socket_write) {
    *output = {};
    output.write_fd = write_fd;
    output.socket_write = socket_write;
    if (catch err = output.write_mu.init()) {
        output.write_mu_ready = false;
    } else {
        output.write_mu_ready = true;
    }
}

fn void repl_server_output_destroy(ReplServerOutput* output) {
    if (output == null) return;
    *output = {};
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
    char* preload_project_dir = null
) {
    *conn = {};
    conn.next_session_id = 1;
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
    usz next_cap = conn.session_capacity > 0 ? conn.session_capacity * 2 : 4;
    while (next_cap < needed) next_cap *= 2;
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
    if (slot == conn.session_capacity && !repl_server_ensure_session_capacity(conn, conn.session_capacity + 1)) {
        return null;
    }
    if (slot == old_capacity) slot = old_capacity;

    char[REPL_SERVER_SESSION_ID_MAX] id_buf = {};
    if (catch err = io::bprintf(&id_buf, "s%d", (int)conn.next_session_id)) {
        (void)err;
        return null;
    }
    char[] id = io::bprintf(&id_buf, "s%d", (int)conn.next_session_id)!!;
    conn.next_session_id++;

    char* stored_id = (char*)mem::malloc(id.len + 1);
    if (stored_id == null) return null;
    mem::copy(stored_id, id.ptr, id.len);
    stored_id[id.len] = 0;

    conn.sessions[slot] = {
        .active = true,
        .ready = false,
        .id_ptr = stored_id,
        .id_len = id.len,
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

fn void repl_server_capture_reset(Interp* interp) {
    interp.console_capture_len = 0;
    if (interp.console_capture_buf != null) interp.console_capture_buf[0] = 0;
}

fn char[] repl_server_capture_text(Interp* interp) {
    if (interp.console_capture_buf == null || interp.console_capture_len == 0) return "";
    return interp.console_capture_buf[:interp.console_capture_len];
}

fn bool repl_server_eval_result_interrupted(EvalResult result) @inline {
    return result.error.has_error && json_repl_str_eq(json_repl_error_message(result), "evaluation interrupted");
}

fn bool repl_server_send_eval_result(ReplServerOutput* output, char[] id, char[] session, EvalResult result, Interp* interp) {
    char[] captured = repl_server_capture_text(interp);
    if (captured.len > 0 && !repl_server_send_out_event(output, id, session, captured)) return false;

    if (repl_server_eval_result_interrupted(result)) {
        return repl_server_send_interrupted_event(output, id, session);
    }

    if (result.error.has_error) {
        return repl_server_send_event_error(
            output,
            id,
            session,
            "runtime/eval-error",
            json_repl_error_message(result),
            true,
            json_repl_zero_based(result.error.line),
            json_repl_zero_based(result.error.column)
        );
    }

    char* rendered = null;
    usz rendered_len = 0;
    if (!json_repl_render_value(result.value, &interp.symbols, &rendered, &rendered_len)) {
        return repl_server_send_event_error(
            output,
            id,
            session,
            "runtime/render-failed",
            "failed to render evaluation result"
        );
    }
    defer mem::free(rendered);

    if (!repl_server_send_value_event(output, id, session, rendered[:rendered_len])) return false;
    return repl_server_send_done(output, id, session);
}

fn bool repl_server_preload_project_session(ReplServerOutput* output, ReplServerSession* session, char[] id, char* project_dir) {
    char[1280] main_path_buf = {};
    usz main_path_len = 0;
    if (!main::repl_resolve_project_main_path(project_dir, main_path_buf[..], &main_path_len)) {
        return repl_server_send_event_error(
            output,
            id,
            session.id_ptr[:session.id_len],
            "runtime/project-preload-failed",
            "failed to resolve Omni project entry"
        );
    }

    char[] path = main_path_buf[:main_path_len];
    if (try source = io::file::load_temp((String)path)) {
        Interp* interp = session.interp;
        if (!push_source_dir(path, interp)) {
            return repl_server_send_event_error(
                output,
                id,
                session.id_ptr[:session.id_len],
                "runtime/source-dir-allocation-failed",
                "project preload: failed to allocate source directory context"
            );
        }
        defer pop_source_dir(interp);

        interp.console_capture_active = true;
        repl_server_capture_reset(interp);
        EvalResult result = run_program(source, interp);
        interp.console_capture_active = false;

        char[] captured = repl_server_capture_text(interp);
        if (captured.len > 0 && !repl_server_send_out_event(output, id, session.id_ptr[:session.id_len], captured)) {
            return false;
        }
        if (repl_server_eval_result_interrupted(result)) {
            return repl_server_send_interrupted_event(output, id, session.id_ptr[:session.id_len]);
        }
        if (result.error.has_error) {
            return repl_server_send_event_error(
                output,
                id,
                session.id_ptr[:session.id_len],
                "runtime/project-preload-failed",
                json_repl_error_message(result),
                true,
                json_repl_zero_based(result.error.line),
                json_repl_zero_based(result.error.column)
            );
        }
        return true;
    }

    main::EntryFileReadKind read_kind;
    if (!main::classify_file_read_path(path, &read_kind)) {
        char[128] msg_buf = {};
        char[] message = io::bprintf(
            &msg_buf,
            "project preload: %s",
            (ZString)main::file_read_kind_reason(read_kind)
        )!!;
        return repl_server_send_event_error(
            output,
            id,
            session.id_ptr[:session.id_len],
            main::file_read_kind_code(read_kind),
            message
        );
    }

    return repl_server_send_event_error(
        output,
        id,
        session.id_ptr[:session.id_len],
        "io/read-failed",
        "project preload: read failed"
    );
}

fn bool repl_server_eval_session(ReplServerOutput* output, ReplServerSession* session, char[] id, char[] code, bool program_mode) {
    Interp* interp = session.interp;
    interp.console_capture_active = true;
    repl_server_capture_reset(interp);

    EvalResult result = program_mode
        ? repl_eval_program(code, interp)
        : repl_eval_line(code, interp);

    interp.console_capture_active = false;
    return repl_server_send_eval_result(output, id, session.id_ptr[:session.id_len], result, interp);
}

fn bool repl_server_load_file_session(ReplServerOutput* output, ReplServerSession* session, char[] id, char[] path) {
    if (try source = io::file::load_temp((String)path)) {
        Interp* interp = session.interp;
        if (!push_source_dir(path, interp)) {
            return repl_server_send_event_error(
                output,
                id,
                session.id_ptr[:session.id_len],
                "runtime/source-dir-allocation-failed",
                "load-file: failed to allocate source directory context"
            );
        }
        defer pop_source_dir(interp);

        interp.console_capture_active = true;
        repl_server_capture_reset(interp);
        EvalResult result = run_program(source, interp);
        interp.console_capture_active = false;

        char[] captured = repl_server_capture_text(interp);
        if (captured.len > 0 && !repl_server_send_out_event(output, id, session.id_ptr[:session.id_len], captured)) return false;
        if (repl_server_eval_result_interrupted(result)) {
            return repl_server_send_interrupted_event(output, id, session.id_ptr[:session.id_len]);
        }
        if (result.error.has_error) {
            return repl_server_send_event_error(
                output,
                id,
                session.id_ptr[:session.id_len],
                "runtime/eval-error",
                json_repl_error_message(result),
                true,
                json_repl_zero_based(result.error.line),
                json_repl_zero_based(result.error.column)
            );
        }

        char* rendered = null;
        usz rendered_len = 0;
        if (!json_repl_render_value(result.value, &interp.symbols, &rendered, &rendered_len)) {
            return repl_server_send_event_error(
                output,
                id,
                session.id_ptr[:session.id_len],
                "runtime/render-failed",
                "failed to render evaluation result"
            );
        }
        defer mem::free(rendered);

        if (!repl_server_send_value_event(output, id, session.id_ptr[:session.id_len], rendered[:rendered_len])) return false;
        if (!repl_server_send_loaded_event(output, id, session.id_ptr[:session.id_len], path)) return false;
        return repl_server_send_done(output, id, session.id_ptr[:session.id_len]);
    }

    main::EntryFileReadKind read_kind;
    if (!main::classify_file_read_path(path, &read_kind)) {
        char[128] msg_buf = {};
        char[] message = io::bprintf(
            &msg_buf,
            "load-file: %s",
            (ZString)main::file_read_kind_reason(read_kind)
        )!!;
        return repl_server_send_event_error(
            output,
            id,
            session.id_ptr[:session.id_len],
            main::file_read_kind_code(read_kind),
            message
        );
    }

    return repl_server_send_event_error(
        output,
        id,
        session.id_ptr[:session.id_len],
        "io/read-failed",
        "load-file: read failed"
    );
}

fn bool repl_server_complete_session(ReplServerOutput* output, ReplServerSession* session, char[] id, char[] prefix) {
    char[] session_id = session.id_ptr[:session.id_len];
    if (!repl_server_send_complete_event(output, id, session_id, session.interp.global_env, &session.interp.symbols, prefix)) {
        return false;
    }
    return repl_server_send_done(output, id, session_id);
}

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

fn void repl_server_worker_run_command(ReplServerWorker* worker, ReplServerCommand* command) {
    if (worker == null || command == null) return;
    ReplServerOutput* output = worker.output;
    ReplServerSession* session = command.session;
    if (session != null) session.interrupt_requested.store(0);

    switch (command.kind) {
        case REPL_SERVER_CMD_CLONE: {
            if (session == null) {
                (void)repl_server_send_protocol_error(output, command.id_ptr[:command.id_len], "runtime/session-create-failed", "failed to create session");
                return;
            }
            Interp* interp = null;
            if (!repl_server_create_interp(&interp)) {
                (void)repl_server_send_protocol_error(output, command.id_ptr[:command.id_len], "runtime/session-create-failed", "failed to create session");
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
            if (!repl_server_send_session_event(output, command.id_ptr[:command.id_len], session_id)) return;
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

    @pool_init(&allocator::LIBC_ALLOCATOR, 65536)
    {
        main::thread_registry_init();
        defer main::thread_registry_shutdown();

        while (true) {
            worker.mu.lock();
            while (!worker.command_pending && !worker.stop_requested) {
                (void)worker.cv.wait(&worker.mu);
            }
            if (worker.stop_requested && !worker.command_pending) {
                worker.mu.unlock();
                break;
            }

            ReplServerCommand command = worker.pending_command;
            worker.pending_command = {};
            worker.command_pending = false;
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

fn bool repl_server_handle_request(ReplServerConnection* conn, ReplServerRequest* request, char[] transport_name) {
    ReplServerOutput* output = &conn.output;
    ReplServerWorker* worker = &conn.worker;
    char[] id = request.id;

    if (json_repl_str_eq(request.op, "describe")) {
        if (!repl_server_send_describe(output, id, transport_name)) return false;
        return repl_server_send_done(output, id, "");
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

fn void repl_server_handle_stream(
    int read_fd,
    int write_fd,
    bool socket_write,
    char[] transport_name,
    bool preload_project = false,
    char* preload_project_dir = null
) {
    ReplServerConnection conn = {};
    repl_server_connection_init(&conn, write_fd, socket_write, preload_project, preload_project_dir);
    defer repl_server_connection_destroy(&conn);

    if (!repl_server_worker_start(&conn.worker)) {
        (void)repl_server_send_protocol_error(&conn.output, "", "runtime/server-init-failed", "failed to start repl worker");
        return;
    }
    defer repl_server_worker_shutdown(&conn.worker);

    char[REPL_SERVER_LINE_MAX] line_buf = {};
    while (true) {
        ReplServerReadResult read = repl_server_read_line_fd(read_fd, line_buf[..], socket_write);
        if (read.failed) break;
        if (read.len == 0 && read.eof) break;
        if (read.truncated) {
            if (!repl_server_send_protocol_error(&conn.output, "", "protocol/request-too-large", "input line exceeds supported length")) break;
            continue;
        }
        if (read.len == 0) continue;

        ReplServerRequest request = repl_server_parse_request(line_buf[:read.len]);
        defer repl_server_request_free(&request);
        if (!request.ok) {
            char[] id = request.has_id ? request.id : "";
            if (!repl_server_send_protocol_error(&conn.output, id, "protocol/invalid-request", request.error_message)) break;
            continue;
        }

        if (!repl_server_handle_request(&conn, &request, transport_name)) break;
    }
}

fn int repl_server_stdio(bool preload_project = false, char* preload_project_dir = null) {
    main::thread_registry_init();
    defer main::thread_registry_shutdown();
    repl_server_handle_stream(0, 1, false, "stdio", preload_project, preload_project_dir);
    return 0;
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

    while (true) {
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

    while (true) {
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
        repl_server_handle_stream(client_fd, client_fd, true, "tcp", preload_project, preload_project_dir);
        c_close_fd(client_fd);
    }

    return 0;
}
