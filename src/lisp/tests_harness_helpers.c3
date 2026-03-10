module lisp;

import std::io;
import main;

fn void setup(Interp* interp, char[] code) {
    run(code, interp);
}

fn bool jit_checks_enabled() @inline {
    // Generated JIT code is not ASAN-instrumented.
    return !main::stack_runtime_asan_enabled();
}

fn Value* test_copy_from_releasing_scope(Value* v, Interp* interp, main::ScopeRegion* releasing_scope, uint site) {
    if (v == null) return null;
    if (releasing_scope == null) return v;
    if (!boundary_ptr_in_scope((void*)v, releasing_scope)) return v;
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.releasing_scope = releasing_scope;
    return boundary_copy_to_parent_site_ctx(v, interp, site, interp.active_promotion_ctx);
}

fn void test_eq(Interp* interp, char[] name, char[] expr, long expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_int(ri.value) && ri.value.int_val == expected;

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && jv.tag == INT && jv.int_val == expected;
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_eq_double(Interp* interp, char[] name, char[] expr, double expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_double(ri.value) && ri.value.double_val == expected;

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && is_double(jv) && jv.double_val == expected;
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_truthy(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && !is_nil(ri.value) && !(is_symbol(ri.value) && ri.value.sym_val == interp.sym_false);

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && !is_nil(jv) && !(is_symbol(jv) && jv.sym_val == interp.sym_false);
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_nil(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_nil(ri.value);

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && is_nil(jv);
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_error(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = ri.error.has_error;
    if (interp_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (expected error)", (ZString)name);
        (*fail)++;
    }
}

fn void test_error_contains(Interp* interp, char[] name, char[] expr_str, char[] expected_substr, int* pass, int* fail) {
    EvalResult ri = run(expr_str, interp);
    if (!ri.error.has_error) {
        io::printfn("[FAIL] %s (expected error, got success)", (ZString)name);
        (*fail)++; return;
    }
    // Check if error message contains expected substring
    char[] msg = ri.error.message[:256];
    bool found = false;
    usz slen = 0;
    while (slen < 256 && msg[slen] != 0) slen++;
    usz elen = expected_substr.len;
    if (elen <= slen) {
        for (usz i = 0; i <= slen - elen; i++) {
            bool match = true;
            for (usz j = 0; j < elen; j++) {
                if (msg[i + j] != expected_substr[j]) { match = false; break; }
            }
            if (match) { found = true; break; }
        }
    }
    if (found) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (error '%s' doesn't contain '%s')", (ZString)name, (ZString)msg, (ZString)expected_substr);
        (*fail)++;
    }
}

fn bool str_starts_with(char[] haystack, char[] needle) {
    if (haystack.len < needle.len) return false;
    for (usz i = 0; i < needle.len; i++) {
        if (haystack[i] != needle[i]) return false;
    }
    return true;
}

fn void test_str_val(Interp* interp, char[] name, char[] expr, char[] expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_string(ri.value) &&
        str_starts_with(ri.value.str_chars[:ri.value.str_len], expected) &&
        ri.value.str_len == expected.len;

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && jv.tag == STRING &&
                    str_starts_with(jv.str_chars[:jv.str_len], expected) &&
                    jv.str_len == expected.len;
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        char[256] got_buf;
        char[] got_str = "?";
        if (!ri.error.has_error && is_string(ri.value)) {
            got_str = ri.value.str_chars[:ri.value.str_len];
        }
        io::printfn("[FAIL] %s (interp=%s, jit=%s, got='%s')", (ZString)name,
            interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL", (ZString)got_str);
        (*fail)++;
    }
}

fn void test_str(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_string(ri.value);

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && jv.tag == STRING;
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_tag(Interp* interp, char[] name, char[] expr, ValueTag expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && ri.value != null && ri.value.tag == expected;

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && jv.tag == expected;
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

fn void test_gt(Interp* interp, char[] name, char[] expr, long min, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_int(ri.value) && ri.value.int_val > min;

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        Expr* ast = parse_for_jit(expr, interp);
        if (ast != null) {
            JitFn f = jit_compile(ast, interp);
            if (f != null) {
                Value* jv = jit_exec(f, interp);
                jit_ok = jv != null && jv.tag == INT && jv.int_val > min;
            }
        }
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}

// Interp-only: for tests that involve stateful side effects or features
// the JIT cannot handle (modules, error paths, println, etc.)
fn void test_eq_interp(Interp* interp, char[] name, char[] expr, long expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    if (!ri.error.has_error && is_int(ri.value) && ri.value.int_val == expected) {
        emit_pass(name);
        (*pass)++;
    } else {
        if (!ri.error.has_error && is_int(ri.value)) { io::printfn("[FAIL] %s (got %d)", (ZString)name, ri.value.int_val); }
        else { io::printfn("[FAIL] %s", (ZString)name); }
        (*fail)++;
    }
}

fn void test_truthy_interp(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    if (!ri.error.has_error && !is_nil(ri.value) && !(is_symbol(ri.value) && ri.value.sym_val == interp.sym_false)) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s", (ZString)name);
        (*fail)++;
    }
}

fn void test_nil_interp(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    if (!ri.error.has_error && is_nil(ri.value)) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s", (ZString)name);
        (*fail)++;
    }
}

fn void test_str_interp(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    if (!ri.error.has_error && is_string(ri.value)) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s", (ZString)name);
        (*fail)++;
    }
}

fn void test_tag_interp(Interp* interp, char[] name, char[] expr, ValueTag expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    if (!ri.error.has_error && ri.value != null && ri.value.tag == expected) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s", (ZString)name);
        (*fail)++;
    }
}

// JIT-only: for stateful tests that would double-mutate if run through both interp and JIT
fn void test_eq_jit(Interp* interp, char[] name, char[] source, long expected, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        test_eq_interp(interp, name, source, expected, pass, fail);
        return;
    }
    Expr* ast = parse_for_jit(source, interp);
    if (ast == null) { (*fail)++; io::printfn("[FAIL] %s (JIT parse error)", (ZString)name); return; }
    JitFn f = jit_compile(ast, interp);
    if (f == null) { (*fail)++; io::printfn("[FAIL] %s (JIT compile error)", (ZString)name); return; }
    Value* result = jit_exec(f, interp);
    if (result != null && result.tag == INT && result.int_val == expected) {
        emit_pass(name);
        (*pass)++;
    } else {
        (*fail)++;
        if (result != null && result.tag == INT) { io::printfn("[FAIL] %s (JIT got %d)", (ZString)name, result.int_val); }
        else { io::printfn("[FAIL] %s", (ZString)name); }
    }
}

fn void test_nil_jit(Interp* interp, char[] name, char[] source, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        test_nil_interp(interp, name, source, pass, fail);
        return;
    }
    Expr* ast = parse_for_jit(source, interp);
    if (ast == null) { (*fail)++; io::printfn("[FAIL] %s (JIT parse error)", (ZString)name); return; }
    JitFn f = jit_compile(ast, interp);
    if (f == null) { (*fail)++; io::printfn("[FAIL] %s (JIT compile error)", (ZString)name); return; }
    Value* result = jit_exec(f, interp);
    if (result == null || is_nil(result)) {
        emit_pass(name);
        (*pass)++;
    } else {
        (*fail)++;
        io::printfn("[FAIL] %s (JIT expected nil)", (ZString)name);
    }
}

fn double dabs(double x) @inline { return x < 0.0 ? -x : x; }

fn bool value_matches_double(Value* v, double expected) @inline {
    return v != null && is_number(v) && dabs(to_double(v) - expected) < 0.0001;
}

fn bool eval_expr_as_double(Interp* interp, char[] expr, double expected) {
    EvalResult ri = run(expr, interp);
    if (ri.error.has_error) return false;
    return value_matches_double(ri.value, expected);
}

fn bool eval_jit_as_double(Interp* interp, char[] expr, double expected) {
    Expr* ast = parse_for_jit(expr, interp);
    if (ast == null) return false;
    JitFn f = jit_compile(ast, interp);
    if (f == null) return false;
    Value* jv = jit_exec(f, interp);
    return value_matches_double(jv, expected);
}

fn void test_double(Interp* interp, char[] name, char[] expr, double expected, int* pass, int* fail) {
    bool interp_ok = eval_expr_as_double(interp, expr, expected);
    bool jit_ok = !jit_checks_enabled() || eval_jit_as_double(interp, expr, expected);

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        (*fail)++;
    }
}


fn Value* make_root_int_for_test(Interp* interp, long n) {
    main::ScopeRegion* saved = interp.current_scope;
    interp.current_scope = interp.root_scope;
    Value* v = make_int(interp, n);
    interp.current_scope = saved;
    return v;
}

fn bool delta_in_range(usz delta, usz min, usz max) @inline {
    return delta >= min && delta <= max;
}
