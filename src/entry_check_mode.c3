module main;

import std::io;
import lisp;

extern fn char* c_getcwd(char* buf, usz size) @extern("getcwd");

const char[] CHECK_COMPILE_UNSUPPORTED_MARKER = "unsupported expr type";

fn char* find_check_input_file(int argc, char** argv, int check_idx) {
    for (int i = check_idx + 1; i < argc; i++) {
        if (str_eq(argv[i], "--json")) continue;
        return argv[i];
    }
    return null;
}

fn char[] check_resolve_path(char* input_file, char[] abs_buf) {
    char[] input = input_file[:cstr_len(input_file)];
    if (input.len == 0) return input;
    if (input[0] == '/') return input;

    char* cwd = c_getcwd(&abs_buf[0], abs_buf.len);
    if (cwd == null) return input;

    usz cwd_len = cstr_len(cwd);
    if (cwd_len + 1 + input.len > abs_buf.len) return input;
    abs_buf[cwd_len] = '/';
    for (usz i = 0; i < input.len; i++) {
        abs_buf[cwd_len + 1 + i] = input[i];
    }
    return abs_buf[:cwd_len + 1 + input.len];
}

fn lisp::EvalResult check_compile_source(char[] source, lisp::Interp* interp) {
    char[] c3_code = lisp::compile_to_c3(source, interp);
    if (c3_code.len == 0) {
        return lisp::eval_error("compiler/lowering failed");
    }
    if (cstr_contains(c3_code, CHECK_COMPILE_UNSUPPORTED_MARKER)) {
        return lisp::eval_error("compiler/lowering failed: unsupported expr type");
    }
    return lisp::eval_ok(null);
}

fn lisp::EvalResult check_parse_source(char[] source, lisp::Interp* interp) {
    lisp::Lexer lex;
    lex.init(source);
    defer lex.destroy();

    lisp::Parser p;
    p.init(&lex, interp);

    while (!lex.at_end() && !p.has_error) {
        lisp::Expr* _expr = p.parse_expr();
        if (_expr == null && !p.has_error) break;
    }

    if (p.has_error) {
        lisp::EvalResult r;
        r.value = null;
        r.error.has_error = true;
        r.error.line = p.error_line;
        r.error.column = p.error_col;
        usz len = p.error_msg_len;
        if (len > 255) len = 255;
        for (usz i = 0; i < len; i++) {
            r.error.message[i] = p.error_msg[i];
        }
        r.error.message[len] = 0;
        return r;
    }

    return lisp::eval_ok(null);
}

fn int run_check_mode(int argc, char** argv, int check_idx) {
    bool json = find_flag_index(argc, argv, "--json") >= 0;
    char* input_file = find_check_input_file(argc, argv, check_idx);
    if (input_file == null) {
        if (json) {
            print_check_usage_json();
            return 1;
        }
        return print_check_usage();
    }

    char[] input_path = input_file[:cstr_len(input_file)];
    char[4096] abs_buf = {};
    char[] resolved_path = check_resolve_path(input_file, abs_buf[..]);

    if (try source = io::file::load_temp((String)input_path)) {
        thread_registry_init();

        lisp::Interp* interp = (lisp::Interp*)mem::malloc(lisp::Interp.sizeof);
        interp.init();
        lisp::register_primitives(interp);
        lisp::register_stdlib(interp);

        lisp::EvalResult result = check_parse_source(source, interp);
        char[] diag_code = JSON_PARSE_ERROR_CODE;

        if (!result.error.has_error) {
            result = check_compile_source(source, interp);
            diag_code = JSON_COMPILE_ERROR_CODE;
        }

        interp.destroy();
        mem::free(interp);
        thread_registry_shutdown();

        if (json) {
            print_check_result_json(resolved_path, result, diag_code);
        } else {
            print_check_result_text(resolved_path, result);
        }
        return result.error.has_error ? 1 : 0;
    }

    if (json) {
        print_check_read_error_json(resolved_path);
    } else {
        print_check_read_error(input_file);
    }
    return 1;
}
