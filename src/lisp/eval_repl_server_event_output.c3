module lisp;

import std::core::mem;
import std::io;
import std::thread;

struct ReplServerOutput {
    int write_fd;
    bool socket_write;
    thread::Mutex write_mu;
    bool write_mu_ready;
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
    if (try rendered = io::bprintf(&temp_path_buf, "%s.tmp.%d", (ZString)final_path, c_getpid())) {
        temp_path = rendered;
    } else {
        return false;
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
    if (try rendered = io::bprintf(&text_buf, "%d\n", port)) {
        text = rendered;
    } else {
        return false;
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
