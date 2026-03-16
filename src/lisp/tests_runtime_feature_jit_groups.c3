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
        emit_pass_literal("[PASS] jit policy: retired code tombstones track and clear by attach serial");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: retired code tombstones track and clear by attach serial");
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

fn void run_jit_policy_multi_interp_lifetime_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: multi-interp lifetime guard skipped when jit checks disabled");
        (*pass)++;
        return;
    }

    Expr* primary_ast = parse_for_jit("(+ 2 3)", interp);
    bool ok = primary_ast != null;
    JitCompiledFn primary_fn = jit_compiled_fn_null();
    if (ok) {
        primary_fn = jit_compile(primary_ast, interp);
        ok = !jit_compiled_fn_is_null(primary_fn) && g_jit_initialized;
    }

    Interp* other = null;
    if (ok) {
        other = (Interp*)mem::malloc(Interp.sizeof);
        ok = other != null;
    }
    if (ok) {
        other.init();
        Expr* other_ast = parse_for_jit("(* 2 3)", other);
        ok = other_ast != null;
        if (ok) {
            JitCompiledFn other_fn = jit_compile(other_ast, other);
            ok = !jit_compiled_fn_is_null(other_fn);
        }
    }

    if (other != null) {
        other.destroy();
        mem::free(other);
    }

    if (ok) {
        ok = g_jit_initialized && g_jit_interp_refcount > 0;
    }

    Value* value = null;
    if (ok) {
        value = jit_exec(primary_fn, interp);
        ok = value != null && value.tag == INT && value.int_val == 5;
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: destroying one interp preserves JIT code for other live interp");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: destroying one interp preserves JIT code for other live interp");
        (*fail)++;
    }
}

fn void run_jit_policy_continuation_teardown_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    Interp* other = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = other != null;
    if (ok) other.init();

    EvalResult r = {};
    if (ok) {
        r = run("(checkpoint (capture k k))", other);
        ok = !r.error.has_error &&
             r.value != null &&
             r.value.tag == CONTINUATION &&
             other.stack_ctx_pool.all_count > 0;
    }

    if (other != null) {
        other.destroy();
        ok = ok &&
             other.stack_ctx_pool.all_count == 0 &&
             other.stack_ctx_pool.pool_size == 0;
        mem::free(other);
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: interp teardown reclaims abandoned continuation stack contexts");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: interp teardown reclaims abandoned continuation stack contexts");
        (*fail)++;
    }
}

fn void run_jit_policy_shared_handle_state_teardown_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    Interp* other = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = other != null;
    if (ok) other.init();

    if (ok) {
        HandleEffectState* shared_state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
        ok = shared_state != null;
        if (ok) {
            handle_effect_state_init(shared_state, other);
            shared_state.continuation_refcount = 1;

            // Order matters: continuation list is LIFO, so allocate non-retained first
            // and retained second to force retained->non-retained traversal at teardown.
            Continuation* non_retained = other.alloc_lisp_continuation();
            Continuation* retained = other.alloc_lisp_continuation();
            ok = non_retained != null && retained != null;
            if (ok) {
                non_retained.handle_state = shared_state;
                non_retained.handle_retained = false;
                retained.handle_state = shared_state;
                retained.handle_retained = true;
            } else {
                handle_effect_state_release(shared_state);
            }
        }
    }

    if (other != null) {
        other.destroy();
        ok = ok &&
             other.stack_ctx_pool.all_count == 0 &&
             other.stack_ctx_pool.pool_size == 0;
        mem::free(other);
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: interp teardown safely handles shared handle-state continuations");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: interp teardown safely handles shared handle-state continuations");
        (*fail)++;
    }
}

fn void run_jit_policy_cross_interp_continuation_guard_test(Interp* interp, int* pass, int* fail) {
    Interp* other = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = other != null;
    if (ok) other.init();

    EvalResult captured = {};
    if (ok) {
        captured = run("(checkpoint (capture k k))", other);
        ok = !captured.error.has_error &&
             captured.value != null &&
             captured.value.tag == CONTINUATION;
    }

    if (ok) {
        Value* resumed = jit_apply_value(captured.value, make_nil(interp), interp);
        ok = resumed != null &&
             resumed.tag == ERROR &&
             str_starts_with(resumed.str_chars[:resumed.str_len], "cannot resume continuation from a different interpreter");
    }

    if (other != null) {
        other.destroy();
        mem::free(other);
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: cross-interpreter continuation resume is rejected");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: cross-interpreter continuation resume is rejected");
        (*fail)++;
    }
}

fn void run_jit_policy_escaped_handle_continuation_guard_test(Interp* interp, int* pass, int* fail) {
    EvalResult escaped = run(
        "(let (k (handle (signal ask 1) (ask x (with-continuation kk kk)))) (k 42))",
        interp
    );
    bool escaped_rejected = false;
    if (escaped.error.has_error) {
        escaped_rejected = str_starts_with(escaped.error.message[:256], "resolve: continuation escaped an inactive handle frame");
    } else if (escaped.value != null && escaped.value.tag == ERROR) {
        escaped_rejected = str_starts_with(escaped.value.str_chars[:escaped.value.str_len], "resolve: continuation escaped an inactive handle frame");
    }

    EvalResult followup = run("(+ 1 1)", interp);
    bool state_ok = !followup.error.has_error &&
                    followup.value != null &&
                    followup.value.tag == INT &&
                    followup.value.int_val == 2;

    bool ok = escaped_rejected && state_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: escaped handle continuation is rejected without corrupting eval state");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: escaped handle continuation is rejected without corrupting eval state");
        (*fail)++;
    }
}

fn void run_jit_policy_side_effect_escaped_handle_continuation_guard_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    Interp* other = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = other != null;
    if (ok) other.init();

    HandleEffectState* state = null;
    Continuation* k = null;
    main::StackCtx* ctx = null;
    if (ok) {
        state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
        ok = state != null;
    }
    if (ok) {
        handle_effect_state_init(state, other);
        k = other.alloc_lisp_continuation();
        ctx = main::stack_ctx_create(&other.stack_ctx_pool);
        ok = k != null && ctx != null;
    }
    if (ok) {
        k.ctx = ctx;
        k.handle_state = state;
        k.handle_retained = true;
        handle_effect_state_release(state);
        ok = k.ctx == null && k.handle_state == null && !k.handle_retained;
    }
    if (ctx != null) {
        main::stack_ctx_destroy(ctx, &other.stack_ctx_pool);
    }
    if (other != null) {
        other.destroy();
        mem::free(other);
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: side-effect escaped handle continuation is rejected safely");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: side-effect escaped handle continuation is rejected safely");
        (*fail)++;
    }
}

fn void run_jit_policy_multishot_capture_scope_guard_clone_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: multi-shot capture clone guard skipped when jit checks disabled");
        (*pass)++;
        return;
    }

    EvalResult r = run(
        "(let (k (checkpoint (let (x 10) (+ x (capture kk kk))))) (+ (k 1) (k 2)))",
        interp
    );
    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == INT &&
              r.value.int_val == 23;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: multi-shot capture clone preserves scope-guard isolation");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: multi-shot capture clone preserves scope-guard isolation");
        (*fail)++;
    }
}

fn void run_jit_policy_shadowed_primitive_call_test(Interp* interp, int* pass, int* fail) {
    EvalResult local_let = run("(let (+ (lambda (a b) 99)) (+ 1 2))", interp);
    bool let_ok = !local_let.error.has_error &&
                  local_let.value != null &&
                  local_let.value.tag == INT &&
                  local_let.value.int_val == 99;

    EvalResult arg_shadow = run("((lambda (+) (+ 1 2)) (lambda (a b) 88))", interp);
    bool arg_ok = !arg_shadow.error.has_error &&
                  arg_shadow.value != null &&
                  arg_shadow.value.tag == INT &&
                  arg_shadow.value.int_val == 88;

    bool ok = let_ok && arg_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: shadowed arithmetic symbols do not use direct primitive fast path");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: shadowed arithmetic symbols do not use direct primitive fast path");
        (*fail)++;
    }
}

fn void run_jit_policy_capture_boundary_reset_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run("(block (checkpoint 1) (capture k k))", interp);
    bool outside_rejected = false;
    if (r.error.has_error) {
        outside_rejected = str_contains(r.error.message[:256], "capture outside of checkpoint");
    } else if (r.value != null && r.value.tag == ERROR) {
        outside_rejected = str_contains(r.value.str_chars[:r.value.str_len], "capture outside of checkpoint");
    }

    EvalResult followup = run("(+ 2 3)", interp);
    bool followup_ok = !followup.error.has_error &&
                       followup.value != null &&
                       followup.value.tag == INT &&
                       followup.value.int_val == 5;

    bool ok = outside_rejected && followup_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: checkpoint boundary does not leak reset state into later capture");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: checkpoint boundary does not leak reset state into later capture");
        (*fail)++;
    }
}

fn void run_jit_policy_tco_persistent_parent_root_rewrite_test(Interp* interp, int* pass, int* fail) {
    // Invariant: JIT TCO env-copy keeps root-persistent mutable-box identity but
    // rewrites transient parents into root-owned lifetime, not short-lived call scopes.
    main::ScopeRegion* saved = interp.current_scope;
    main::ScopeRegion* source = main::scope_create(saved);
    main::ScopeRegion* target = main::scope_create(saved);
    if (source == null || target == null) {
        if (source != null) main::scope_release(source);
        if (target != null) main::scope_release(target);
        io::printn("[FAIL] jit policy: tco persistent parent rewrite lands in root scope");
        (*fail)++;
        return;
    }

    SymbolId payload_sym = interp.symbols.intern("__jit_policy_tco_parent_payload");
    SymbolId box_sym = interp.symbols.intern("__jit_policy_tco_parent_box");

    interp.current_scope = source;
    Env* transient_parent = make_env(interp, interp.global_env);
    env_define_with_barrier(transient_parent, payload_sym, make_int(interp, 313), interp);

    Env* persistent_box = boundary_env_extend_in_root(
        interp,
        transient_parent,
        box_sym,
        make_root_int_for_test(interp, 5)
    );
    Env* original_parent = persistent_box != null ? persistent_box.parent : null;

    interp.current_scope = target;
    Env* copied = jit_copy_tco_env_chain_for_recycle(persistent_box, interp, source);

    bool copied_ok = copied != null &&
                     copied == persistent_box &&
                     copied.is_root_persistent_box();
    bool rewritten_ok = copied_ok &&
                       copied.parent != null &&
                       copied.parent != original_parent &&
                       main::is_in_scope((void*)copied.parent, interp.root_scope) &&
                       !main::is_in_scope((void*)copied.parent, target);

    Value* before_release = rewritten_ok ? copied.lookup(payload_sym) : null;
    main::scope_release(source);
    Value* after_source_release = rewritten_ok ? copied.lookup(payload_sym) : null;

    interp.current_scope = saved;
    main::scope_release(target);
    Value* after_target_release = rewritten_ok ? copied.lookup(payload_sym) : null;

    bool value_ok = before_release != null &&
                    before_release.tag == INT &&
                    before_release.int_val == 313 &&
                    after_source_release != null &&
                    after_source_release.tag == INT &&
                    after_source_release.int_val == 313 &&
                    after_target_release != null &&
                    after_target_release.tag == INT &&
                    after_target_release.int_val == 313;
    bool ok = copied_ok && rewritten_ok && value_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: tco persistent parent rewrite lands in root scope");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: tco persistent parent rewrite lands in root scope (copied_ok=%s rewritten_ok=%s value_ok=%s)",
            copied_ok ? "yes" : "no",
            rewritten_ok ? "yes" : "no",
            value_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- JIT Policy Tests ---");

    bool saved_jit_enabled = interp.flags.jit_enabled;
    bool parsed = run_jit_policy_warm_cache_tests(interp, pass, fail);
    if (parsed) {
        run_jit_policy_cache_overwrite_count_test(interp, pass, fail);
        run_jit_policy_cache_probe_saturation_recovery_test(interp, pass, fail);
        run_jit_policy_retired_code_tombstone_table_test(interp, pass, fail);
        run_jit_policy_gc_safe_point_test(interp, pass, fail);
        run_jit_policy_stale_compiled_fn_guard_test(interp, pass, fail);
        run_jit_policy_foreign_compiled_fn_owner_test(interp, pass, fail);
        run_jit_policy_boundary_reset_test(interp, pass, fail);
        run_jit_policy_stale_raise_scrub_test(interp, pass, fail);
        run_jit_policy_variadic_no_fallback_test(interp, pass, fail);
        run_jit_policy_multi_interp_lifetime_test(interp, pass, fail);
        run_jit_policy_continuation_teardown_test(interp, pass, fail);
        run_jit_policy_shared_handle_state_teardown_test(interp, pass, fail);
        run_jit_policy_cross_interp_continuation_guard_test(interp, pass, fail);
        run_jit_policy_escaped_handle_continuation_guard_test(interp, pass, fail);
        run_jit_policy_side_effect_escaped_handle_continuation_guard_test(interp, pass, fail);
        run_jit_policy_multishot_capture_scope_guard_clone_test(interp, pass, fail);
        run_jit_policy_shadowed_primitive_call_test(interp, pass, fail);
        run_jit_policy_capture_boundary_reset_test(interp, pass, fail);
        run_jit_policy_tco_persistent_parent_root_rewrite_test(interp, pass, fail);
    }

    interp.flags.jit_enabled = saved_jit_enabled;
    jit_cache_clear();
}

fn void run_jit_policy_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_jit_policy_tests, pass, fail);
}
