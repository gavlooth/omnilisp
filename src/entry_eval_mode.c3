module main;

import std::io;
import lisp;

const usz EVAL_RENDER_INITIAL_CAP = 256;
const usz EVAL_RENDER_MAX_CAP = 16 * 1024 * 1024;

fn char* find_eval_source_arg(int argc, char** argv, int eval_idx) {
    for (int i = eval_idx + 1; i < argc; i++) {
        if (str_eq(argv[i], "--json")) continue;
        if (cli_arg_starts_with_double_dash(argv[i])) return null;
        return argv[i];
    }
    return null;
}

fn char* find_eval_option_like_arg(int argc, char** argv, int eval_idx) {
    for (int i = eval_idx + 1; i < argc; i++) {
        if (str_eq(argv[i], "--json")) continue;
        if (cli_arg_starts_with_double_dash(argv[i])) return argv[i];
        return null;
    }
    return null;
}

fn lisp::Interp* create_runtime_interp() {
    thread_registry_init();
    lisp::Interp* interp = create_plain_interp();
    if (interp == null) {
        thread_registry_shutdown();
        return null;
    }
    if (!lisp::bootstrap_runtime_interp(interp)) {
        destroy_plain_interp(interp);
        thread_registry_shutdown();
        return null;
    }
    interp.flags.jit_enabled = true;
    return interp;
}

fn void destroy_runtime_interp(lisp::Interp* interp) {
    destroy_plain_interp(interp);
    if (mem_telemetry_enabled()) {
        lisp::runtime_memory_print_telemetry();
    }
    thread_registry_shutdown();
}

fn bool render_eval_value(lisp::Value* value, lisp::SymbolTable* syms, char** out_buf, usz* out_len) {
    *out_buf = null;
    *out_len = 0;

    usz cap = EVAL_RENDER_INITIAL_CAP;
    char* buf = (char*)mem::malloc(cap);
    if (buf == null) return false;

    while (true) {
        usz rendered = lisp::print_value_to_buf(value, syms, buf, cap);
        if (rendered + 1 < cap) {
            *out_buf = buf;
            *out_len = rendered;
            return true;
        }

        if (cap >= EVAL_RENDER_MAX_CAP) {
            mem::free(buf);
            return false;
        }

        usz next_cap = cap * 2;
        if (next_cap > EVAL_RENDER_MAX_CAP) next_cap = EVAL_RENDER_MAX_CAP;
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

fn int run_eval_mode(int argc, char** argv, int eval_idx) {
    bool json = find_flag_index(argc, argv, "--json") >= 0;
    char* source_arg = find_eval_source_arg(argc, argv, eval_idx);
    if (source_arg == null) {
        char* unexpected = find_eval_option_like_arg(argc, argv, eval_idx);
        if (unexpected != null) {
            if (json) {
                print_eval_unexpected_arg_json(unexpected);
                return 1;
            }
            return print_unexpected_mode_arg("--eval", unexpected);
        }
        if (json) {
            print_eval_usage_json();
            return 1;
        }
        return print_eval_usage();
    }
    char* extra = find_extra_mode_arg(argc, argv, eval_idx, 1, true);
    if (extra != null) {
        if (json) {
            print_eval_unexpected_arg_json(extra);
            return 1;
        }
        return print_unexpected_mode_arg("--eval", extra);
    }

    char[] source = source_arg[:cstr_len(source_arg)];
    lisp::Interp* interp = create_runtime_interp();
    if (interp == null) {
        lisp::EvalResult result = lisp::eval_error("failed to create runtime interpreter for --eval");
        if (json) {
            print_eval_result_json(source, result, null);
        } else {
            print_eval_result_text(result, null);
        }
        return 1;
    }
    defer destroy_runtime_interp(interp);

    if (json) interp.flags.console_to_stderr = true;

    lisp::EvalResult result = lisp::run(source, interp);

    if (json) {
        print_eval_result_json(source, result, interp);
    } else {
        print_eval_result_text(result, interp);
    }
    return result.error.has_error ? 1 : 0;
}
