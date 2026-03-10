module lisp;


import std::io;

fn bool run_jit_policy_warm_cache_tests(Interp* interp, int* pass, int* fail) {
    Expr* ast = parse_for_jit("(lambda (x) x)", interp);
    if (ast == null) {
        io::printn("[FAIL] jit policy: parse fixture");
        (*fail)++;
        return false;
    }

    jit_cache_clear();
    interp.flags.jit_enabled = false;
    jit_cache_expr(ast, interp);
    JitFn cached = jit_cache_lookup(ast);
    if (cached == null) {
        emit_pass_literal("[PASS] jit policy: warm-cache skips when jit disabled");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: warm-cache skips when jit disabled");
        (*fail)++;
    }

    if (!jit_checks_enabled()) return true;

    interp.flags.jit_enabled = true;
    jit_cache_expr(ast, interp);
    cached = jit_cache_lookup(ast);
    if (cached != null) {
        emit_pass_literal("[PASS] jit policy: warm-cache stores when jit enabled");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: warm-cache stores when jit enabled");
        (*fail)++;
    }
    return true;
}

fn void run_jit_policy_gc_safe_point_test(Interp* interp, int* pass, int* fail) {
    Expr* gc_ast = parse_for_jit("(+ 1 2)", interp);
    bool gc_ok = gc_ast != null;
    if (gc_ok) {
        JitFn f0 = jit_compile(gc_ast, interp);
        gc_ok = f0 != null;
    }

    usz before = g_jit_state_count;
    if (gc_ok) {
        g_jit_gc_needed = true;
        JitFn f1 = jit_compile(gc_ast, interp);
        gc_ok = f1 != null && g_jit_state_count > before;
    }

    if (gc_ok) {
        g_jit_gc_needed = true;
        jit_gc();
        gc_ok = (g_jit_state_count == 0) && !g_jit_gc_needed;
    }

    if (gc_ok) {
        emit_pass_literal("[PASS] jit policy: gc runs only at explicit safe points");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: gc runs only at explicit safe points");
        (*fail)++;
    }
}

fn void run_jit_policy_boundary_reset_test(Interp* interp, int* pass, int* fail) {
    Expr* reset_ast = parse_for_jit("(* 2 3)", interp);
    bool reset_ok = reset_ast != null;
    if (reset_ok) {
        JitFn f = jit_compile(reset_ast, interp);
        reset_ok = f != null && g_jit_state_count > 0;
    }

    if (reset_ok) {
        interp.flags.raise_pending = true;
        interp.raise_msg_len = 1;
        interp.last_call_name = interp.sym_lambda;
        run_test_group_boundary_reset(interp);
        reset_ok = g_jit_state_count == 0 &&
                  !g_jit_gc_needed &&
                  !interp.flags.raise_pending &&
                  interp.raise_msg_len == 0 &&
                  interp.raise_payload == null &&
                  (uint)interp.last_call_name == 0;
    }

    if (reset_ok) {
        emit_pass_literal("[PASS] jit policy: group boundary reset clears transient runtime state");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: group boundary reset clears transient runtime state");
        (*fail)++;
    }
}

fn void run_jit_policy_stale_raise_scrub_test(Interp* interp, int* pass, int* fail) {
    // Regression: stale pending raise must not leak into a new top-level run.
    interp.flags.raise_pending = true;
    interp.raise_msg_len = 3;
    interp.raise_msg[0] = 's';
    interp.raise_msg[1] = 't';
    interp.raise_msg[2] = 'e';
    interp.raise_msg[3] = 0;

    EvalResult r = run(
        "(handle (let loop (n 5 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc 1)))) (raise msg 999))",
        interp
    );

    bool raise_scrub_ok = !r.error.has_error &&
                         r.value != null &&
                         r.value.tag == INT &&
                         r.value.int_val == 5 &&
                         !interp.flags.raise_pending &&
                         interp.raise_msg_len == 0 &&
                         interp.raise_payload == null;
    if (raise_scrub_ok) {
        emit_pass_literal("[PASS] jit policy: top-level run scrubs stale pending raise state");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: top-level run scrubs stale pending raise state");
        (*fail)++;
    }
}

fn void run_jit_policy_variadic_no_fallback_test(Interp* interp, int* pass, int* fail) {
    usz before_total = jit_fallback_audit_total();
    EvalResult r = run("((lambda (h .. rest) (length rest)) 1 2 3 4)", interp);
    usz after_total = jit_fallback_audit_total();

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == INT &&
              r.value.int_val == 3 &&
              after_total == before_total;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: variadic lambda executes without jit fallback");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: variadic lambda executes without jit fallback (before=%d after=%d err=%d)",
            (long)before_total,
            (long)after_total,
            r.error.has_error ? 1 : 0
        );
        (*fail)++;
    }
}

fn void run_jit_policy_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- JIT Policy Tests ---");

    bool saved_jit_enabled = interp.flags.jit_enabled;
    bool parsed = run_jit_policy_warm_cache_tests(interp, pass, fail);
    if (parsed) {
        run_jit_policy_gc_safe_point_test(interp, pass, fail);
        run_jit_policy_boundary_reset_test(interp, pass, fail);
        run_jit_policy_stale_raise_scrub_test(interp, pass, fail);
        run_jit_policy_variadic_no_fallback_test(interp, pass, fail);
    }

    interp.flags.jit_enabled = saved_jit_enabled;
    jit_cache_clear();
}

fn void run_jit_policy_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_jit_policy_tests, pass, fail);
}
