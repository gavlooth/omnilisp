module main;

import std::io;

const char[] JSON_DESCRIBE_USAGE_CODE = "cli/usage";
const char[] JSON_DESCRIBE_UNKNOWN_CODE = "cli/describe-unknown-symbol";
const char[] JSON_DESCRIBE_ERROR_SEVERITY = "error";

fn int print_describe_usage() {
    io::printn("Usage: omni --describe [--json] <symbol>");
    return 1;
}

fn void print_describe_usage_json() {
    io::print("{\"ok\":false,\"code\":");
    print_json_escaped_string(JSON_DESCRIBE_USAGE_CODE);
    io::print(",\"severity\":");
    print_json_escaped_string(JSON_DESCRIBE_ERROR_SEVERITY);
    io::print(",\"message\":");
    print_json_escaped_string("usage: omni --describe [--json] <symbol>");
    io::print("}\n");
}

fn void print_describe_unexpected_arg_json(char* arg) {
    io::print("{\"ok\":false,\"code\":");
    print_json_escaped_string(JSON_DESCRIBE_USAGE_CODE);
    io::print(",\"severity\":");
    print_json_escaped_string(JSON_DESCRIBE_ERROR_SEVERITY);
    io::print(",\"message\":");
    char[256] msg_buf;
    if (try msg = io::bprintf(&msg_buf, "unexpected argument after --describe: %s", (ZString)arg)) {
        print_json_escaped_string(msg);
    } else {
        print_json_escaped_string("unexpected argument after --describe");
    }
    io::print("}\n");
}

fn void print_describe_result_text(char[] symbol, SymbolDescribeInfo info) {
    if (!info.found) {
        io::printfn("Unknown symbol: %s", (ZString)symbol);
        return;
    }
    io::printfn("%s", (ZString)symbol);
    io::printfn("kind: %s", (ZString)info.kind);
    io::printfn("documentation: %s", (ZString)info.documentation);
}

fn void print_describe_result_json(char[] symbol, SymbolDescribeInfo info) {
    if (!info.found) {
        io::print("{\"ok\":false,\"symbol\":");
        print_json_escaped_string(symbol);
        io::print(",\"error\":{\"code\":");
        print_json_escaped_string(JSON_DESCRIBE_UNKNOWN_CODE);
        io::print(",\"severity\":");
        print_json_escaped_string(JSON_DESCRIBE_ERROR_SEVERITY);
        io::print(",\"message\":");
        print_json_escaped_string("unknown symbol");
        io::print("}}\n");
        return;
    }

    io::print("{\"ok\":true,\"symbol\":");
    print_json_escaped_string(symbol);
    io::print(",\"kind\":");
    print_json_escaped_string(info.kind);
    io::print(",\"documentation\":");
    print_json_escaped_string(info.documentation);
    io::print(",\"error\":null}\n");
}
