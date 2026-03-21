module lisp;

import std::io;

fn void deduce_test_expect_true_symbol(
    Interp* interp,
    char[] source,
    char[] pass_msg,
    char[] fail_msg,
    int* pass,
    int* fail
) {
    EvalResult r = run(source, interp);
    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal(pass_msg);
        (*pass)++;
        return;
    }
    io::printn((ZString)fail_msg);
    (*fail)++;
}

fn void deduce_test_expect_error(
    Interp* interp,
    char[] source,
    char[] pass_msg,
    char[] fail_msg,
    int* pass,
    int* fail
) {
    EvalResult r = run(source, interp);
    if (r.error.has_error) {
        emit_pass_literal(pass_msg);
        (*pass)++;
        return;
    }
    io::printn((ZString)fail_msg);
    (*fail)++;
}

fn void deduce_test_expect_int_at_least(
    Interp* interp,
    char[] source,
    long min_value,
    char[] pass_msg,
    char[] fail_fmt,
    int* pass,
    int* fail
) {
    EvalResult r = run(source, interp);
    if (!r.error.has_error && is_int(r.value) && r.value.int_val >= min_value) {
        emit_pass_literal(pass_msg);
        (*pass)++;
        return;
    }
    io::printfn((String)fail_fmt, is_int(r.value) ? (int)r.value.int_val : -1);
    (*fail)++;
}

fn void deduce_test_expect_int_exact(
    Interp* interp,
    char[] source,
    long expected,
    char[] pass_msg,
    char[] fail_fmt,
    int* pass,
    int* fail
) {
    EvalResult r = run(source, interp);
    if (!r.error.has_error && is_int(r.value) && r.value.int_val == expected) {
        emit_pass_literal(pass_msg);
        (*pass)++;
        return;
    }
    io::printfn((String)fail_fmt, is_int(r.value) ? (int)r.value.int_val : -1);
    (*fail)++;
}

fn void deduce_test_expect_string(
    Interp* interp,
    char[] source,
    char[] pass_msg,
    char[] fail_msg,
    int* pass,
    int* fail
) {
    EvalResult r = run(source, interp);
    if (!r.error.has_error && is_string(r.value)) {
        emit_pass_literal(pass_msg);
        (*pass)++;
        return;
    }
    io::printn((ZString)fail_msg);
    (*fail)++;
}

fn void deduce_test_expect_void(
    Interp* interp,
    char[] source,
    char[] pass_msg,
    char[] fail_fmt,
    int* pass,
    int* fail
) {
    EvalResult r = run(source, interp);
    if (!r.error.has_error && r.value != null && r.value.tag == VOID) {
        emit_pass_literal(pass_msg);
        (*pass)++;
        return;
    }
    io::printfn((String)fail_fmt, r.value != null ? (int)r.value.tag : -1);
    (*fail)++;
}
