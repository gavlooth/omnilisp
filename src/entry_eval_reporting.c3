module main;

import std::io;
import lisp;

const char[] JSON_RUNTIME_ERROR_CODE = "runtime/eval-error";
const char[] JSON_RUNTIME_RENDER_ERROR_CODE = "runtime/render-failed";

fn int print_eval_usage() {
    io::printn("Usage: omni --eval [--json] '<expr>'");
    return 1;
}

fn void print_eval_usage_json() {
    io::print("{\"ok\":false,\"code\":");
    print_json_escaped_string("cli/usage");
    io::print(",\"severity\":");
    print_json_escaped_string("error");
    io::print(",\"message\":");
    print_json_escaped_string("usage: omni --eval [--json] '<expr>'");
    io::print("}\n");
}

fn char[] eval_error_message(lisp::EvalResult result) {
    usz len = 0;
    while (len < 256 && result.error.message[len] != 0) len++;
    return result.error.message[:len];
}

fn void print_eval_result_text(lisp::EvalResult result, lisp::Interp* interp) {
    if (result.error.has_error) {
        if (result.error.line > 0) {
            io::printf("Error at line %d, column %d: ", (int)result.error.line, (int)result.error.column);
        } else {
            io::print("Error: ");
        }
        char[] message = eval_error_message(result);
        foreach (c : message) io::printf("%c", c);
        io::printn("");
        return;
    }

    lisp::print_value(result.value, &interp.symbols);
    io::printn("");
}

fn void print_eval_result_json(char[] source, lisp::EvalResult result, lisp::Interp* interp) {
    io::print("{\"ok\":");
    io::print(result.error.has_error ? "false" : "true");
    io::print(",\"input\":");
    print_json_escaped_string(source);

    if (result.error.has_error) {
        char[] message = eval_error_message(result);
        usz line = check_zero_based_pos(result.error.line);
        usz column = check_zero_based_pos(result.error.column);
        io::print(",\"value\":null,\"error\":{\"code\":");
        print_json_escaped_string(JSON_RUNTIME_ERROR_CODE);
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
        io::print("}\n");
        return;
    }

    char* rendered = null;
    usz rendered_len = 0;
    if (!render_eval_value(result.value, &interp.symbols, &rendered, &rendered_len)) {
        io::print(",\"value\":null,\"error\":{\"code\":");
        print_json_escaped_string(JSON_RUNTIME_RENDER_ERROR_CODE);
        io::print(",\"message\":");
        print_json_escaped_string("failed to render evaluation result");
        io::print("}}\n");
        return;
    }
    defer mem::free(rendered);

    io::print(",\"value\":");
    print_json_escaped_string(rendered[:rendered_len]);
    io::print(",\"error\":null}\n");
}
