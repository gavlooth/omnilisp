module lisp;

import std::io;

fn bool scheduler_test_str_contains(char[] haystack, char[] needle) {
    if (needle.len == 0) return false;
    if (haystack.len < needle.len) return false;
    for (usz i = 0; i <= haystack.len - needle.len; i++) {
        bool ok = true;
        for (usz j = 0; j < needle.len; j++) {
            if (haystack[i + j] != needle[j]) {
                ok = false;
                break;
            }
        }
        if (ok) return true;
    }
    return false;
}

fn bool scheduler_test_eval_is_error(EvalResult r) @inline {
    return r.error.has_error || (r.value != null && r.value.tag == ERROR);
}

fn bool scheduler_test_eval_is_truthy_success(EvalResult r) @inline {
    return !r.error.has_error &&
           r.value != null &&
           r.value.tag != ERROR &&
           r.value.tag != NIL;
}

fn void scheduler_test_expect_error(
    Interp* interp,
    char[] source,
    char[] pass_msg,
    char[] fail_msg,
    int* pass,
    int* fail
) {
    EvalResult r = run(source, interp);
    if (scheduler_test_eval_is_error(r)) {
        emit_pass_literal(pass_msg);
        (*pass)++;
        return;
    }
    io::printn((ZString)fail_msg);
    (*fail)++;
}

fn void scheduler_test_expect_truthy_success(
    Interp* interp,
    char[] source,
    char[] pass_msg,
    char[] fail_msg,
    int* pass,
    int* fail
) {
    EvalResult r = run(source, interp);
    if (scheduler_test_eval_is_truthy_success(r)) {
        emit_pass_literal(pass_msg);
        (*pass)++;
        return;
    }
    io::printn((ZString)fail_msg);
    (*fail)++;
}

fn void scheduler_test_expect_int_exact(
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

fn void scheduler_test_expect_void(
    Interp* interp,
    char[] source,
    char[] pass_msg,
    char[] fail_msg,
    int* pass,
    int* fail
) {
    EvalResult r = run(source, interp);
    if (!r.error.has_error && r.value != null && r.value.tag == VOID) {
        emit_pass_literal(pass_msg);
        (*pass)++;
        return;
    }
    io::printn((ZString)fail_msg);
    (*fail)++;
}

fn void scheduler_test_finish_bool(
    bool ok,
    char[] pass_msg,
    char[] fail_msg,
    int* pass,
    int* fail
) {
    if (ok) {
        emit_pass_literal(pass_msg);
        (*pass)++;
        return;
    }
    io::printn((ZString)fail_msg);
    (*fail)++;
}

fn void scheduler_test_finish_step_phase(
    bool ok,
    usz failed_step,
    char[] failed_phase,
    char[] pass_msg,
    char[] fail_fmt,
    int* pass,
    int* fail
) {
    if (ok) {
        emit_pass_literal(pass_msg);
        (*pass)++;
        return;
    }
    io::printfn((String)fail_fmt, (int)failed_step, (ZString)failed_phase);
    (*fail)++;
}

fn void scheduler_test_finish_phase(
    bool ok,
    char[] failed_phase,
    char[] pass_msg,
    char[] fail_fmt,
    int* pass,
    int* fail
) {
    if (ok) {
        emit_pass_literal(pass_msg);
        (*pass)++;
        return;
    }
    io::printfn((String)fail_fmt, (ZString)failed_phase);
    (*fail)++;
}
