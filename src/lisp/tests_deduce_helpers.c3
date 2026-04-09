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

fn bool deduce_test_capture_analyze_raise_payload(
    Interp* interp,
    Value* db_val,
    Value** out_payload
) {
    if (out_payload != null) *out_payload = null;
    if (interp == null || db_val == null) return false;

    if (!interp.ensure_handler_capacity(interp.handler_count + 1)) return false;
    SymbolId[1] handler_tags = { interp.sym_raise };
    EffectHandler handler = {
        .tags = &handler_tags[0],
        .clauses = null,
        .closures = null,
        .clause_count = 1,
        .handler_env = null,
        .body_expr = null,
        .body_env = null,
        .strict_mode = false,
        .effect_state = null,
    };
    interp.handler_stack[interp.handler_count++] = handler;

    Value*[1] analyze_args = { db_val };
    Value* analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
    interp.handler_count--;

    bool ok = analyzed != null &&
              analyzed.tag == ERROR &&
              interp.flags.raise_pending &&
              interp.raise_payload != null &&
              interp.raise_payload.tag == HASHMAP &&
              interp.raise_payload.hashmap_val != null;
    if (ok && out_payload != null) *out_payload = interp.raise_payload;
    return ok;
}

fn bool deduce_test_string_equals(Value* value, char[] expected) {
    if (!is_string(value)) return false;
    if (value.str_len != expected.len) return false;
    for (usz i = 0; i < expected.len; i++) {
        if (value.str_chars[i] != expected[i]) return false;
    }
    return true;
}
