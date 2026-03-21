module lisp;

import std::io;

fn bool compiler_code_contains_all(char[] code, char[][] needles) {
    foreach (needle : needles) {
        if (!str_contains(code, needle)) return false;
    }
    return true;
}

fn bool compiler_code_contains_none(char[] code, char[][] needles) {
    foreach (needle : needles) {
        if (str_contains(code, needle)) return false;
    }
    return true;
}

fn void compiler_test_finish_bool(
    bool ok,
    char[] pass_msg,
    char[] fail_msg,
    usz* pass,
    usz* fail
) {
    if (ok) {
        (*pass)++;
        compiler_print_pass(pass_msg);
        return;
    }
    (*fail)++;
    io::printn((ZString)fail_msg);
}
