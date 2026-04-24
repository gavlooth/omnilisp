module lisp;

import std::io;
import std::core::mem;
import main;

fn void memory_lifetime_test_free_opaque_payload(void* ptr) {
    if (ptr != null) mem::free(ptr);
}

fn int* memory_lifetime_test_opaque_payload(main::ScopeRegion* owner_scope) {
    if (owner_scope == null) return null;
    int* payload = (int*)mem::malloc(int.sizeof);
    if (payload == null) return null;
    main::scope_register_dtor(owner_scope, (void*)payload, &memory_lifetime_test_free_opaque_payload);
    return payload;
}

fn void setup(Interp* interp, char[] code) {
    EvalResult result = run(code, interp);
    if (!result.error.has_error) return;

    io::printfn(
        "[FAIL] test setup failed: %s at %d:%d",
        (ZString)&result.error.message,
        (int)result.error.line,
        (int)result.error.column
    );
    if (code.len > 0) {
        io::printfn("[FAIL] test setup source (%d bytes): %s", (int)code.len, (ZString)&code[0]);
    }
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
    BoundaryCopyResult copied = boundary_copy_to_parent_site_ctx_checked(
        v,
        interp,
        site,
        interp.active_promotion_ctx
    );
    if (!boundary_copy_result_ok(copied)) {
        return boundary_raise(interp, "boundary/copy-fault", (String)boundary_copy_fault_message(copied.fault_code));
    }
    return copied.value;
}

fn Value* test_copy_to_parent(Value* v, Interp* interp) {
    BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;
    Value* copied = copy_to_parent_with_fault(v, interp, &copy_fault);
    if (copy_fault != BOUNDARY_COPY_FAULT_NONE) {
        return boundary_raise(interp, "boundary/copy-fault", (String)boundary_copy_fault_message(copy_fault));
    }
    return copied;
}

fn void print_jit_failure_debug(Interp* interp, char[] name, char[] stage, Value* value) {
    if (value != null && value.tag == ERROR) {
        io::printfn("  JIT debug [%s]: stage=%s error=%s",
            (ZString)name,
            (ZString)stage,
            (ZString)value.str_chars);
        return;
    }

    char[256] rendered = {};
    usz len = print_value_to_buf(value, &interp.symbols, &rendered, rendered.len);
    if (len > 0) {
        io::printfn("  JIT debug [%s]: stage=%s value=%s",
            (ZString)name,
            (ZString)stage,
            (ZString)&rendered);
    } else {
        io::printfn("  JIT debug [%s]: stage=%s value=<empty>",
            (ZString)name,
            (ZString)stage);
    }
}

struct JitTopLevelEval {
    bool ok;
    char[] stage;
    Value* value;
}

fn JitTopLevelEval eval_jit_top_level(Interp* interp, char[] source) {
    JitTopLevelEval out = {
        .ok = false,
        .stage = "parse",
        .value = null,
    };
    if (interp == null) {
        out.stage = "interp";
        return out;
    }

    run_clear_stale_raise_state(interp);
    run_clear_stale_jit_tco_state(interp);
    defer run_clear_stale_jit_tco_state(interp);
    dispatch_pending_signal_callbacks(interp);
    runtime_gc_between_top_level_runs();

    Expr* ast = parse_for_jit(source, interp);
    if (ast == null) return out;

    ast = expand_macros_in_expr(ast, interp);

    out.stage = "scope";
    main::ScopeRegion* saved_scope = null;
    main::ScopeRegion* child_scope = boundary_push_child_scope(interp, &saved_scope);
    if (child_scope == null) return out;

    out.stage = "compile";
    RuntimeCompiledExpr compiled = runtime_compile_expr(ast, interp);
    if (runtime_compiled_expr_is_null(compiled)) {
        boundary_pop_child_scope(interp, saved_scope, child_scope);
        return out;
    }

    out.stage = "exec";
    Value* result = runtime_exec_compiled_expr(compiled, interp);
    bool child_spliced = false;
    result = run_promote_result(result, interp, child_scope, saved_scope, &child_spliced);
    main::ScopeRegion* release_scope = child_spliced ? null : child_scope;
    boundary_pop_child_scope(interp, saved_scope, release_scope);
    dispatch_pending_signal_callbacks(interp);

    out.ok = true;
    out.stage = "ok";
    out.value = result;
    return out;
}

fn void test_eq(Interp* interp, char[] name, char[] expr, long expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_int(ri.value) && ri.value.int_val == expected;

    bool jit_ok = !jit_checks_enabled();
    char[] jit_stage = "disabled";
    Value* jit_value = null;
    if (jit_checks_enabled()) {
        JitTopLevelEval jit_eval = eval_jit_top_level(interp, expr);
        jit_stage = jit_eval.stage;
        jit_value = jit_eval.value;
        jit_ok = jit_eval.ok && jit_value != null && jit_value.tag == INT && jit_value.int_val == expected;
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        if (!jit_ok && jit_checks_enabled()) print_jit_failure_debug(interp, name, jit_stage, jit_value);
        (*fail)++;
    }
}

fn void test_eq_double(Interp* interp, char[] name, char[] expr, double expected, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_double(ri.value) && ri.value.double_val == expected;

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        JitTopLevelEval jit_eval = eval_jit_top_level(interp, expr);
        Value* jv = jit_eval.value;
        jit_ok = jit_eval.ok && jv != null && is_double(jv) && jv.double_val == expected;
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
    char[] jit_stage = "disabled";
    Value* jit_value = null;
    if (jit_checks_enabled()) {
        JitTopLevelEval jit_eval = eval_jit_top_level(interp, expr);
        jit_stage = jit_eval.stage;
        jit_value = jit_eval.value;
        jit_ok = jit_eval.ok &&
            jit_value != null &&
            !is_nil(jit_value) &&
            !(is_symbol(jit_value) && jit_value.sym_val == interp.sym_false);
    }

    if (interp_ok && jit_ok) {
        emit_pass(name);
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)name, interp_ok ? "ok" : "FAIL", jit_ok ? "ok" : "FAIL");
        if (!jit_ok && jit_checks_enabled()) print_jit_failure_debug(interp, name, jit_stage, jit_value);
        (*fail)++;
    }
}

fn void test_nil(Interp* interp, char[] name, char[] expr, int* pass, int* fail) {
    EvalResult ri = run(expr, interp);
    bool interp_ok = !ri.error.has_error && is_nil(ri.value);

    bool jit_ok = !jit_checks_enabled();
    if (jit_checks_enabled()) {
        JitTopLevelEval jit_eval = eval_jit_top_level(interp, expr);
        jit_ok = jit_eval.ok && jit_eval.value != null && is_nil(jit_eval.value);
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
        JitTopLevelEval jit_eval = eval_jit_top_level(interp, expr);
        Value* jv = jit_eval.value;
        jit_ok = jit_eval.ok &&
            jv != null &&
            jv.tag == STRING &&
            str_starts_with(jv.str_chars[:jv.str_len], expected) &&
            jv.str_len == expected.len;
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
        JitTopLevelEval jit_eval = eval_jit_top_level(interp, expr);
        jit_ok = jit_eval.ok && jit_eval.value != null && jit_eval.value.tag == STRING;
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
        JitTopLevelEval jit_eval = eval_jit_top_level(interp, expr);
        jit_ok = jit_eval.ok && jit_eval.value != null && jit_eval.value.tag == expected;
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
        JitTopLevelEval jit_eval = eval_jit_top_level(interp, expr);
        jit_ok = jit_eval.ok && jit_eval.value != null && jit_eval.value.tag == INT && jit_eval.value.int_val > min;
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
    JitTopLevelEval jit_eval = eval_jit_top_level(interp, source);
    if (!jit_eval.ok) { (*fail)++; io::printfn("[FAIL] %s (JIT %s error)", (ZString)name, (ZString)jit_eval.stage); return; }
    Value* result = jit_eval.value;
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
    JitTopLevelEval jit_eval = eval_jit_top_level(interp, source);
    if (!jit_eval.ok) { (*fail)++; io::printfn("[FAIL] %s (JIT %s error)", (ZString)name, (ZString)jit_eval.stage); return; }
    Value* result = jit_eval.value;
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
    JitTopLevelEval jit_eval = eval_jit_top_level(interp, expr);
    if (!jit_eval.ok) return false;
    Value* jv = jit_eval.value;
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
