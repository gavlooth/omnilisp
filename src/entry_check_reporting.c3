module main;

import std::io;
import lisp;

const char[] JSON_ERROR_SEVERITY = "error";
const char[] JSON_PARSE_ERROR_CODE = "parser/syntax-error";
const char[] JSON_COMPILE_ERROR_CODE = "compiler/lowering-error";

fn void print_cli_error_json(char[] code, char[] message) {
    io::print("{\"ok\":false,\"code\":");
    print_json_escaped_string(code);
    io::print(",\"severity\":");
    print_json_escaped_string(JSON_ERROR_SEVERITY);
    io::print(",\"message\":");
    print_json_escaped_string(message);
    io::print("}\n");
}

fn void print_cli_usage_json(char[] message) {
    print_cli_error_json("cli/usage", message);
}

fn void print_cli_unexpected_arg_json(char* mode, char* arg) {
    char[256] msg_buf;
    if (try msg = io::bprintf(&msg_buf, "unexpected argument after %s: %s", (ZString)mode, (ZString)arg)) {
        print_cli_usage_json(msg);
        return;
    }

    char[192] fallback_buf;
    if (try fallback = io::bprintf(&fallback_buf, "unexpected argument after %s", (ZString)mode)) {
        print_cli_usage_json(fallback);
    } else {
        print_cli_usage_json("unexpected argument");
    }
}

fn void print_check_usage_json() {
    print_cli_usage_json("usage: omni --check [--json] <file>");
}

fn void print_check_unexpected_arg_json(char* arg) {
    print_cli_unexpected_arg_json("--check", arg);
}

fn int print_check_usage() {
    io::printn("Usage: omni --check [--json] <file>");
    return 1;
}

fn void print_json_hex_byte(char c) {
    char[] hex = "0123456789abcdef";
    uint byte = (uint)c;
    io::printf("%c%c", hex[(usz)((byte >> 4) & 0xF)], hex[(usz)(byte & 0xF)]);
}

fn void print_json_escaped_string(char[] value) {
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
                uint byte = (uint)c;
                if (byte < 0x20 || byte >= 0x7f) {
                    io::print("\\u00");
                    print_json_hex_byte(c);
                } else {
                    io::printf("%c", c);
                }
        }
    }
    io::print("\"");
}

fn char[] check_error_message(lisp::EvalResult result) {
    usz len = 0;
    while (len < 256 && result.error.message[len] != 0) len++;
    return result.error.message[:len];
}

fn usz check_zero_based_pos(usz pos) @inline {
    if (pos == 0) return 0;
    return pos - 1;
}

fn void print_check_result_text(char[] path, lisp::EvalResult result) {
    if (!result.error.has_error) {
        io::printfn("Check passed: %s", (ZString)path);
        return;
    }
    char[] message = check_error_message(result);
    io::printf("Check failed: %s", (ZString)path);
    if (result.error.line > 0) {
        io::printf(" (%d:%d)", (int)result.error.line, (int)result.error.column);
    }
    io::print(": ");
    for (usz i = 0; i < message.len; i++) {
        io::printf("%c", message[i]);
    }
    io::printn("");
}

fn void print_check_result_json(char[] path, lisp::EvalResult result, char[] code) {
    io::print("{\"ok\":");
    io::print(result.error.has_error ? "false" : "true");
    io::print(",\"path\":");
    print_json_escaped_string(path);
    io::print(",\"diagnostics\":[");
    if (result.error.has_error) {
        char[] message = check_error_message(result);
        usz line = check_zero_based_pos(result.error.line);
        usz column = check_zero_based_pos(result.error.column);
        io::print("{\"code\":");
        print_json_escaped_string(code);
        io::print(",\"severity\":");
        print_json_escaped_string(JSON_ERROR_SEVERITY);
        io::print(",\"message\":");
        print_json_escaped_string(message);
        io::print(",\"range\":{\"start\":{\"line\":");
        io::printf("%d", (int)line);
        io::print(",\"character\":");
        io::printf("%d", (int)column);
        io::print("},\"end\":{\"line\":");
        io::printf("%d", (int)line);
        io::print(",\"character\":");
        io::printf("%d", (int)(column + 1));
        io::print("}}}");
    }
    io::print("]}\n");
}

fn void print_check_read_error_json(char[] path, EntryFileReadKind kind) {
    char[] code = file_read_kind_code(kind);
    char[] reason = file_read_kind_reason(kind);
    io::print("{\"ok\":false,\"path\":");
    print_json_escaped_string(path);
    io::print(",\"diagnostics\":[{\"code\":");
    print_json_escaped_string(code);
    io::print(",\"severity\":");
    print_json_escaped_string(JSON_ERROR_SEVERITY);
    io::print(",\"message\":");
    char[320] msg_buf;
    char[] msg = io::bprintf(&msg_buf, "cannot read input file '%s': %s", (ZString)path, (ZString)reason)!!;
    print_json_escaped_string(msg);
    io::print(",\"range\":{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":1}}}]}\n");
}

fn void print_check_read_error(char[] path, EntryFileReadKind kind) {
    print_file_read_error_text("input file", path, kind);
}
