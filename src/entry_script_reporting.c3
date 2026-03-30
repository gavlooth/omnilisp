module main;

import std::io;
import lisp;

fn void print_script_eval_error(lisp::EvalResult r) {
    usz msg_len = 0;
    while (msg_len < 256 && r.error.message[msg_len] != 0) {
        msg_len++;
    }
    if (r.error.line > 0) {
        io::printf("Error at line %d, column %d: ", (int)r.error.line, (int)r.error.column);
    } else {
        io::print("Error: ");
    }
    for (usz i = 0; i < msg_len; i++) {
        io::printf("%c", r.error.message[i]);
    }
    io::printn("");
}

fn void print_script_result_if_non_nil(lisp::EvalResult r, lisp::Interp* interp) {
    if (lisp::test_quiet_requested()) return;
    if (r.value != null && !lisp::is_nil(r.value)) {
        lisp::print_value(r.value, &interp.symbols);
        io::printn("");
    }
}

fn void print_script_read_error(char* script_file) {
    io::printfn("Error: cannot read script file '%s'", (ZString)script_file);
}
