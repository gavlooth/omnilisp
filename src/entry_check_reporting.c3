module main;

import std::io;
import lisp;

const char[] JSON_ERROR_SEVERITY = "error";
const char[] JSON_PARSE_ERROR_CODE = "parser/syntax-error";
const char[] JSON_COMPILE_ERROR_CODE = "compiler/lowering-error";
const char[] JSON_IO_ERROR_CODE = "io/read-failed";

fn void print_check_usage_json() {
    io::print("{\"ok\":false,\"code\":\"cli/usage\",\"severity\":\"error\",\"message\":");
    print_json_escaped_string("usage: omni --check [--json] <file>");
    io::print("}\n");
}

fn int print_check_usage() {
    io::printn("Usage: omni --check [--json] <file>");
    return 1;
}

fn void print_json_hex_byte(char c) {
    char[] hex = "0123456789abcdef";
    io::printf("%c%c", hex[(usz)(c >> 4) & 0xF], hex[(usz)c & 0xF]);
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
                if (c < 0x20) {
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

fn void print_check_read_error_json(char[] path) {
    io::print("{\"ok\":false,\"path\":");
    print_json_escaped_string(path);
    io::print(",\"diagnostics\":[{\"code\":");
    print_json_escaped_string(JSON_IO_ERROR_CODE);
    io::print(",\"severity\":");
    print_json_escaped_string(JSON_ERROR_SEVERITY);
    io::print(",\"message\":");
    print_json_escaped_string("cannot read input file");
    io::print(",\"range\":{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":1}}}]}\n");
}

fn void print_check_read_error(char* input_file) {
    io::printfn("Error: cannot read input file %s", (ZString)input_file);
}
