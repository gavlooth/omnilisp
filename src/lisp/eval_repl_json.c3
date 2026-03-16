module lisp;

import std::io;
import std::core::mem;

const usz JSON_REPL_RENDER_INITIAL_CAP = 256;
const usz JSON_REPL_RENDER_MAX_CAP = 16 * 1024 * 1024;

struct JsonReplRequest {
    bool ok;
    bool has_id;
    bool has_input;
    bool program_mode;
    char[] id;
    char[] input;
    char[] error_message;
}

fn char[] json_repl_dup_string(char[] src) {
    char* out = (char*)mem::malloc(src.len + 1);
    if (out == null) return {};
    for (usz i = 0; i < src.len; i++) out[i] = src[i];
    out[src.len] = 0;
    return out[:src.len];
}

fn void json_repl_request_free(JsonReplRequest* request) {
    if (request == null) return;
    if (request.has_id && request.id.ptr != null) {
        mem::free(request.id.ptr);
        request.id = "";
        request.has_id = false;
    }
    if (request.has_input && request.input.ptr != null) {
        mem::free(request.input.ptr);
        request.input = "";
        request.has_input = false;
    }
}

fn bool json_repl_str_eq(char[] left, char[] right) @inline {
    if (left.len != right.len) return false;
    for (usz i = 0; i < left.len; i++) {
        if (left[i] != right[i]) return false;
    }
    return true;
}

fn bool json_repl_val_string(YyjsonVal* val, char[]* out) {
    if (omni_yyjson_is_str(val) == 0) return false;
    char* chars = omni_yyjson_get_str(val);
    usz len = omni_yyjson_get_len(val);
    *out = chars[:len];
    return true;
}

fn JsonReplRequest json_repl_parse_request(char[] line) {
    JsonReplRequest request = {
        .ok = false,
        .has_id = false,
        .has_input = false,
        .program_mode = false,
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

    YyjsonVal* input_val = omni_yyjson_obj_getn(root, "input".ptr, 5);
    if (input_val == null) {
        request.error_message = "request field 'input' is required";
        return request;
    }
    char[] parsed_input = "";
    if (!json_repl_val_string(input_val, &parsed_input)) {
        request.error_message = "request field 'input' must be a string";
        return request;
    }
    request.input = json_repl_dup_string(parsed_input);
    if (request.input.ptr == null) {
        request.error_message = "out of memory while copying request field 'input'";
        return request;
    }
    request.has_input = true;

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

fn char[] json_repl_error_message(EvalResult result) {
    usz len = 0;
    while (len < 256 && result.error.message[len] != 0) len++;
    return result.error.message[:len];
}

fn usz json_repl_zero_based(usz pos) @inline {
    if (pos == 0) return 0;
    return pos - 1;
}

fn void json_repl_print_json_hex_byte(char c) {
    char[] hex = "0123456789abcdef";
    io::printf("%c%c", hex[(usz)(c >> 4) & 0xF], hex[(usz)c & 0xF]);
}

fn void json_repl_print_json_escaped_string(char[] value) {
    io::print("\"");
    foreach (c : value) {
        switch (c) {
            case '"':
                io::print("\\\"");
            case '\\':
                io::print("\\\\");
            case '\n':
                io::print("\\n");
            case '\r':
                io::print("\\r");
            case '\t':
                io::print("\\t");
            default:
                if (c < 0x20) {
                    io::print("\\u00");
                    json_repl_print_json_hex_byte(c);
                } else {
                    io::printf("%c", c);
                }
        }
    }
    io::print("\"");
}

fn bool json_repl_render_value(Value* value, SymbolTable* syms, char** out_buf, usz* out_len) {
    *out_buf = null;
    *out_len = 0;

    usz cap = JSON_REPL_RENDER_INITIAL_CAP;
    char* buf = (char*)mem::malloc(cap);
    if (buf == null) return false;

    while (true) {
        usz rendered = print_value_to_buf(value, syms, buf, cap);
        if (rendered + 1 < cap) {
            *out_buf = buf;
            *out_len = rendered;
            return true;
        }
        if (cap >= JSON_REPL_RENDER_MAX_CAP) {
            mem::free(buf);
            return false;
        }
        usz next_cap = cap * 2;
        if (next_cap > JSON_REPL_RENDER_MAX_CAP) next_cap = JSON_REPL_RENDER_MAX_CAP;
        if (next_cap <= cap) {
            mem::free(buf);
            return false;
        }
        mem::free(buf);
        cap = next_cap;
        buf = (char*)mem::malloc(cap);
        if (buf == null) return false;
    }
}

fn void json_repl_print_id(char[] id) {
    if (id.ptr == null || id.len == 0) {
        io::print("null");
        return;
    }
    json_repl_print_json_escaped_string(id);
}

fn void json_repl_print_protocol_error(char[] id, char[] message) {
    io::print("{\"id\":");
    json_repl_print_id(id);
    io::print(",\"ok\":false,\"error\":{\"code\":");
    json_repl_print_json_escaped_string("protocol/invalid-request");
    io::print(",\"message\":");
    json_repl_print_json_escaped_string(message);
    io::print("}}\n");
    (void)io::stdout().flush();
}

fn void json_repl_print_eval_response(char[] id, EvalResult result, Interp* interp) {
    io::print("{\"id\":");
    json_repl_print_json_escaped_string(id);
    io::print(",\"ok\":");
    io::print(result.error.has_error ? "false" : "true");

    if (result.error.has_error) {
        char[] message = json_repl_error_message(result);
        usz line = json_repl_zero_based(result.error.line);
        usz column = json_repl_zero_based(result.error.column);
        io::print(",\"value\":null,\"error\":{\"code\":");
        json_repl_print_json_escaped_string("runtime/eval-error");
        io::print(",\"message\":");
        json_repl_print_json_escaped_string(message);
        io::print(",\"range\":{\"start\":{\"line\":");
        io::printf("%d", (int)line);
        io::print(",\"character\":");
        io::printf("%d", (int)column);
        io::print("},\"end\":{\"line\":");
        io::printf("%d", (int)line);
        io::print(",\"character\":");
        io::printf("%d", (int)(column + 1));
        io::print("}}}}\n");
        (void)io::stdout().flush();
        return;
    }

    char* rendered = null;
    usz rendered_len = 0;
    if (!json_repl_render_value(result.value, &interp.symbols, &rendered, &rendered_len)) {
        io::print(",\"value\":null,\"error\":{\"code\":");
        json_repl_print_json_escaped_string("runtime/render-failed");
        io::print(",\"message\":");
        json_repl_print_json_escaped_string("failed to render evaluation result");
        io::print("}}\n");
        (void)io::stdout().flush();
        return;
    }
    defer mem::free(rendered);

    io::print(",\"value\":");
    json_repl_print_json_escaped_string(rendered[:rendered_len]);
    io::print(",\"error\":null}\n");
    (void)io::stdout().flush();
}

fn void repl_json(Interp* interp) {
    char[65536] line_buf;

    while (true) {
        ReadLineResult read = read_line(line_buf[..]);
        if (read.len == 0 && read.eof) break;
        if (read.truncated) {
            json_repl_print_protocol_error("", "input line exceeds supported length");
            continue;
        }
        if (read.len == 0) continue;

        JsonReplRequest request = json_repl_parse_request(line_buf[:read.len]);
        defer json_repl_request_free(&request);
        if (!request.ok) {
            json_repl_print_protocol_error(request.id, request.error_message);
            continue;
        }

        EvalResult result = request.program_mode
            ? repl_eval_program(request.input, interp)
            : repl_eval_line(request.input, interp);
        json_repl_print_eval_response(request.id, result, interp);
    }
}
