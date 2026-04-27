module lisp;


import std::io;
import std::core::mem;
import main;

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
    JitCompiledFn cached = jit_cache_lookup(ast);
    if (jit_compiled_fn_is_null(cached)) {
        emit_pass_literal("[PASS] jit policy: warm-cache skips when jit disabled");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: warm-cache skips when jit disabled");
        (*fail)++;
    }

    {
        Expr malformed_with = {};
        ExprWithModule malformed_body = {};
        malformed_with.tag = E_WITH_MODULE;
        malformed_with.with_module = &malformed_body;
        malformed_body.body_count = 1;
        malformed_body.body = null;
        jit_warm_expr_cache(&malformed_with, interp);
        bool ok = !expr_contains_shift(&malformed_with) && !expr_contains_perform(&malformed_with);
        if (ok) {
            emit_pass_literal("[PASS] jit policy: warm-cache tolerates missing with-module body list");
            (*pass)++;
        } else {
            io::printn("[FAIL] jit policy: warm-cache tolerates missing with-module body list");
            (*fail)++;
        }
    }

    {
        Expr** body = (Expr**)mem::malloc(Expr*.sizeof);
        bool ok = body != null;
        if (ok) {
            body[0] = null;
            Expr malformed_with = {};
            ExprWithModule malformed_body = {};
            malformed_with.tag = E_WITH_MODULE;
            malformed_with.with_module = &malformed_body;
            malformed_body.body_count = 1;
            malformed_body.body = body;
            jit_warm_expr_cache(&malformed_with, interp);
            ok = !expr_contains_shift(&malformed_with) && !expr_contains_perform(&malformed_with);
        }
        if (body != null) mem::free(body);
        if (ok) {
            emit_pass_literal("[PASS] jit policy: warm-cache tolerates null with-module body elements");
            (*pass)++;
        } else {
            io::printn("[FAIL] jit policy: warm-cache tolerates null with-module body elements");
            (*fail)++;
        }
    }

    {
        Expr invalid_expr = {};
        char* raw = (char*)&invalid_expr;
        raw[0] = (char)127;
        bool ok = expr_contains_shift(&invalid_expr) && expr_contains_perform(&invalid_expr);
        if (ok) {
            emit_pass_literal("[PASS] jit policy: unknown expr tags are continuation-sensitive");
            (*pass)++;
        } else {
            io::printn("[FAIL] jit policy: unknown expr tags are continuation-sensitive");
            (*fail)++;
        }
    }

    if (!jit_checks_enabled()) return true;

    interp.flags.jit_enabled = true;
    jit_cache_expr(ast, interp);
    cached = jit_cache_lookup(ast);
    if (!jit_compiled_fn_is_null(cached)) {
        emit_pass_literal("[PASS] jit policy: warm-cache stores when jit enabled");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: warm-cache stores when jit enabled");
        (*fail)++;
    }
    return true;
}

fn void run_jit_policy_cache_overwrite_count_test(Interp* interp, int* pass, int* fail) {
    Expr* ast = parse_for_jit("(+ 1 2)", interp);
    Expr* other_ast = parse_for_jit("(* 3 4)", interp);
    bool ok = ast != null;
    if (ok) ok = other_ast != null;
    if (ok) {
        jit_cache_clear();
        usz before = g_jit_cache_count;
        JitCompiledFn compiled = jit_compile(ast, interp);
        JitCompiledFn other_compiled = jit_compile(other_ast, interp);
        ok = !jit_compiled_fn_is_null(compiled);
        if (ok) ok = !jit_compiled_fn_is_null(other_compiled);
        if (ok) {
            jit_cache_store(ast, compiled);
            usz after_first = g_jit_cache_count;
            jit_cache_store(ast, compiled);
            usz after_second = g_jit_cache_count;
            ok = before == 0 && after_first == 1 && after_second == 1;
            if (ok) {
                jit_cache_store(other_ast, other_compiled);
                g_jit_cache_count = JIT_CACHE_GC_THRESHOLD;
                jit_cache_store(ast, compiled);
                ok = g_jit_cache_count == JIT_CACHE_GC_THRESHOLD &&
                     jit_compiled_fn_equals(jit_cache_lookup(ast), compiled) &&
                     jit_compiled_fn_equals(jit_cache_lookup(other_ast), other_compiled);
            }
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: cache overwrite preserves count and avoids false full-cache clears");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: cache overwrite preserves count and avoids false full-cache clears (count=%d)",
            (long)g_jit_cache_count
        );
        (*fail)++;
    }
}

fn void run_jit_policy_cache_probe_saturation_recovery_test(Interp* interp, int* pass, int* fail) {
    Expr* source_ast = parse_for_jit("(+ 40 2)", interp);
    bool ok = source_ast != null;
    JitCompiledFn compiled = jit_compiled_fn_null();
    if (ok) {
        compiled = jit_compile(source_ast, interp);
        ok = !jit_compiled_fn_is_null(compiled);
    }

    AstArena arena = {};
    ast_arena_init(&arena);
    defer ast_arena_destroy(&arena);

    if (ok) {
        jit_cache_clear();
        Expr* target = (Expr*)ast_arena_alloc(&arena, Expr.sizeof);
        ok = target != null;
        if (ok) {
            usz idx = (usz)target % JIT_CACHE_SIZE;
            for (usz probe = 0; probe < 16; probe++) {
                Expr* blocker = (Expr*)ast_arena_alloc(&arena, Expr.sizeof);
                ok = blocker != null;
                if (!ok) break;
                usz slot = (idx + probe) % JIT_CACHE_SIZE;
                g_jit_cache[slot].expr = blocker;
                g_jit_cache[slot].compiled = compiled;
            }
            if (ok) {
                g_jit_cache_count = 16;
                jit_cache_store(target, compiled);
                ok = g_jit_cache_count == 1 && jit_compiled_fn_equals(jit_cache_lookup(target), compiled);
            }
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: cache probe saturation clears and retries instead of dropping entry");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: cache probe saturation clears and retries instead of dropping entry (count=%d)",
            (long)g_jit_cache_count
        );
        (*fail)++;
    }
}

fn void run_jit_policy_retired_code_tombstone_table_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    void* fake_code = (void*)0x123450;
    ulong fake_serial = 0xBEEF;
    void* saturated_code = (void*)0xABCDEF;
    ulong saturated_serial = 0xFACE;

    jit_retired_code_reset();
    bool ok = !jit_retired_code_contains(fake_code, fake_serial);
    if (ok) {
        ok = jit_retired_code_insert(fake_code, fake_serial);
    }
    if (ok) {
        ok = jit_retired_code_contains(fake_code, fake_serial);
    }
    if (ok) {
        jit_retired_code_forget_serial(fake_serial);
        ok = !jit_retired_code_contains(fake_code, fake_serial);
    }
    if (ok) {
        g_jit_retired_code_count = JIT_RETIRED_CODE_MAX;
        g_jit_retired_code_saturated = false;
        ok = !jit_retired_code_insert(saturated_code, saturated_serial) &&
            g_jit_retired_code_saturated &&
            jit_retired_code_contains(saturated_code, saturated_serial);
        jit_retired_code_reset();
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: retired code tombstones track, clear, and saturate fail-closed");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: retired code tombstones track, clear, and saturate fail-closed");
        (*fail)++;
    }
}

fn void run_jit_policy_state_spill_limit_test(Interp* interp, int* pass, int* fail) {
    usz saved_state_count = g_jit_state_count;
    usz saved_spill_count = g_jit_spill_count;
    JitStateSpillNode* saved_spill_head = g_jit_spill_states;
    bool saved_gc_needed = runtime_backend_gc_needed();
    bool saved_pool_warned = runtime_backend_pool_warned();
    ulong nonce = 123;

    g_jit_state_count = JIT_STATE_POOL_SIZE;
    g_jit_spill_count = JIT_STATE_SPILL_MAX;
    g_jit_spill_states = null;
    runtime_backend_clear_gc_needed();
    runtime_backend_clear_pool_warned();

    bool tracked = jit_track_compiled_state((void*)0x1234, (void*)0x5678, interp, &nonce);
    bool ok = !tracked &&
              nonce == 0 &&
              g_jit_spill_count == JIT_STATE_SPILL_MAX &&
              g_jit_spill_states == null &&
              runtime_backend_gc_needed();

    g_jit_state_count = saved_state_count;
    g_jit_spill_count = saved_spill_count;
    g_jit_spill_states = saved_spill_head;
    if (saved_gc_needed) {
        runtime_backend_mark_gc_needed();
    } else {
        runtime_backend_clear_gc_needed();
    }
    if (saved_pool_warned) {
        runtime_backend_mark_pool_warned();
    } else {
        runtime_backend_clear_pool_warned();
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: state spill limit fails compile tracking closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: state spill limit fails compile tracking closed (tracked=%s nonce=%d spill_count=%d head=%s gc=%s)",
            tracked ? "yes" : "no",
            (long)nonce,
            (long)g_jit_spill_count,
            g_jit_spill_states == null ? "null" : "present",
            runtime_backend_gc_needed() ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_gc_safe_point_test(Interp* interp, int* pass, int* fail) {
    Expr* gc_ast = parse_for_jit("(+ 1 2)", interp);
    bool gc_ok = gc_ast != null;
    if (gc_ok) {
        JitCompiledFn f0 = jit_compile(gc_ast, interp);
        gc_ok = !jit_compiled_fn_is_null(f0);
    }

    usz before = g_jit_state_count;
    if (gc_ok) {
        g_jit_gc_needed = true;
        JitCompiledFn f1 = jit_compile(gc_ast, interp);
        gc_ok = !jit_compiled_fn_is_null(f1) && g_jit_state_count > before;
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

fn void run_jit_policy_stale_compiled_fn_guard_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: stale compiled fn guard skipped when jit checks disabled");
        (*pass)++;
        return;
    }

    Expr* ast = parse_for_jit("(+ 5 7)", interp);
    bool ok = ast != null;
    JitCompiledFn compiled = jit_compiled_fn_null();
    if (ok) {
        compiled = jit_compile(ast, interp);
        ok = !jit_compiled_fn_is_null(compiled);
    }

    if (ok) {
        g_jit_gc_needed = true;
        jit_gc();
        Value* result = jit_exec(compiled, interp);
        ok = result != null &&
             result.tag == ERROR &&
             str_contains(result.str_chars[:result.str_len], "stale or foreign compiled function pointer");
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: stale compiled function pointers are rejected after jit_gc");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: stale compiled function pointers are rejected after jit_gc");
        (*fail)++;
    }
}

fn void run_jit_policy_foreign_compiled_fn_owner_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: foreign compiled-fn owner guard skipped when jit checks disabled");
        (*pass)++;
        return;
    }

    Interp* foreign = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = foreign != null;
    JitCompiledFn foreign_fn = jit_compiled_fn_null();
    if (ok) {
        foreign.init();
        Expr* foreign_ast = parse_for_jit("(+ 3 9)", foreign);
        ok = foreign_ast != null;
        if (ok) {
            foreign_fn = jit_compile(foreign_ast, foreign);
            ok = !jit_compiled_fn_is_null(foreign_fn);
        }
    }

    if (foreign != null) {
        foreign.destroy();
        mem::free(foreign);
    }

    if (ok) {
        Value* result = jit_exec(foreign_fn, interp);
        ok = result != null &&
             result.tag == ERROR &&
             str_contains(result.str_chars[:result.str_len], "stale or foreign compiled function pointer");
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: compiled functions are rejected after owner-interpreter teardown");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: compiled functions are rejected after owner-interpreter teardown");
        (*fail)++;
    }
}

fn void run_jit_policy_boundary_reset_test(Interp* interp, int* pass, int* fail) {
    Expr* reset_ast = parse_for_jit("(* 2 3)", interp);
    bool reset_ok = reset_ast != null;
    if (reset_ok) {
        JitCompiledFn f = jit_compile(reset_ast, interp);
        reset_ok = !jit_compiled_fn_is_null(f) && g_jit_state_count > 0;
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
