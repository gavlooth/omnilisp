module lisp;

import std::io;
import std::core::mem;

const usz JSON_REPL_RENDER_INITIAL_CAP = 256;
const usz JSON_REPL_RENDER_MAX_CAP = 16 * 1024 * 1024;

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
    uint byte = (uint)c & 0xFF;
    io::printf("%c%c", hex[(usz)((byte >> 4) & 0xF)], hex[(usz)(byte & 0xF)]);
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
                uint byte = (uint)c & 0xFF;
                if (byte < 0x20 || byte >= 0x80) {
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
