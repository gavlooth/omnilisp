module main;

import std::io;
import lisp;

struct ReplServerModeOptions {
    char* socket_path;
    char* tcp_host;
    int tcp_port;
    bool tcp_mode;
    bool stdio_mode;
    bool load_project;
    char* project_dir;
    char* unexpected;
}

fn bool parse_repl_server_port(char* text, int* out_port) {
    if (text == null || text[0] == 0 || out_port == null) return false;
    long value = 0;
    for (int i = 0; text[i] != 0; i++) {
        char ch = text[i];
        if (ch < '0' || ch > '9') return false;
        value = value * 10 + (long)(ch - '0');
        if (value > 65535) return false;
    }
    if (value <= 0) return false;
    *out_port = (int)value;
    return true;
}

fn ReplServerModeOptions parse_repl_server_mode_options(int argc, char** argv, int repl_server_idx) {
    ReplServerModeOptions options = {};
    for (int i = repl_server_idx + 1; i < argc; i++) {
        char* arg = argv[i];
        if (str_eq(arg, "--socket")) {
            if (options.socket_path != null || options.stdio_mode || options.tcp_mode ||
                i + 1 >= argc || cli_arg_starts_with_dash(argv[i + 1])) {
                options.unexpected = arg;
                return options;
            }
            options.socket_path = argv[i + 1];
            i++;
            continue;
        }
        if (str_eq(arg, "--tcp")) {
            if (options.tcp_mode || options.socket_path != null || options.stdio_mode ||
                i + 2 >= argc || cli_arg_starts_with_dash(argv[i + 1]) || cli_arg_starts_with_dash(argv[i + 2])) {
                options.unexpected = arg;
                return options;
            }
            int parsed_port = 0;
            if (!parse_repl_server_port(argv[i + 2], &parsed_port)) {
                options.unexpected = argv[i + 2];
                return options;
            }
            options.tcp_host = argv[i + 1];
            options.tcp_port = parsed_port;
            options.tcp_mode = true;
            i += 2;
            continue;
        }
        if (str_eq(arg, "--stdio")) {
            if (options.stdio_mode || options.socket_path != null || options.tcp_mode) {
                options.unexpected = arg;
                return options;
            }
            options.stdio_mode = true;
            continue;
        }
        if (str_eq(arg, "--project")) {
            if (options.load_project) {
                options.unexpected = arg;
                return options;
            }
            options.load_project = true;
            if (i + 1 < argc && !cli_arg_starts_with_dash(argv[i + 1])) {
                options.project_dir = argv[i + 1];
                i++;
            }
            continue;
        }
        options.unexpected = arg;
        return options;
    }
    return options;
}

fn int print_repl_server_usage() {
    io::printn("Usage: omni --repl-server --socket <path> [--project [dir]]");
    io::printn("       omni --repl-server --tcp <host> <port> [--project [dir]]");
    io::printn("       omni --repl-server --stdio [--project [dir]]");
    return 1;
}

fn int run_repl_server_mode(int argc, char** argv, int repl_server_idx) {
    ReplServerModeOptions options = parse_repl_server_mode_options(argc, argv, repl_server_idx);
    if (options.unexpected != null) {
        return print_unexpected_mode_arg("--repl-server", options.unexpected);
    }
    if (options.socket_path == null && !options.stdio_mode && !options.tcp_mode) {
        return print_repl_server_usage();
    }
    if (options.stdio_mode) return lisp::repl_server_stdio(options.load_project, options.project_dir);
    if (options.tcp_mode) return lisp::repl_server_tcp(options.tcp_host, options.tcp_port, options.load_project, options.project_dir);
    return lisp::repl_server_socket(options.socket_path, options.load_project, options.project_dir);
}
