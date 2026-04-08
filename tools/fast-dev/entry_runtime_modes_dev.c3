module main;

import std::io;
import lisp;

fn int run_repl_mode(int argc, char** argv, int repl_idx) {
    bool json = false;
    for (int i = repl_idx + 1; i < argc; i++) {
        if (str_eq(argv[i], "--json")) {
            json = true;
            break;
        }
    }

    if (!json) {
        io::printn("Starting Lisp REPL...");
    }
    lisp::Interp* interp = create_runtime_interp();
    defer destroy_runtime_interp(interp);

    if (json) {
        lisp::repl_json(interp);
    } else {
        lisp::repl(interp);
    }
    return 0;
}
