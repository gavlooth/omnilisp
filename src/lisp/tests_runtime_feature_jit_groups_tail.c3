module lisp;

import std::io;
import std::core::mem;
import main;

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

fn void run_jit_policy_capture_discard_recycles_stack_ctx_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    Interp* other = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = other != null;
    if (ok) other.init();

    usz before = 0;
    usz after = 0;
    EvalResult r = {};
    if (ok) {
        before = other.stack_ctx_pool.pool_size;
        r = run("(checkpoint (+ 100 (capture k 42)))", other);
        after = other.stack_ctx_pool.pool_size;
        ok = !r.error.has_error &&
             r.value != null &&
             r.value.tag == INT &&
             r.value.int_val == 42 &&
             (main::stack_pool_cache_limit() > 0 ? after == before + 1 : after == before);
    }

    if (other != null) {
        other.destroy();
        mem::free(other);
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: checkpoint capture discard recycles stack ctx");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: checkpoint capture discard recycles stack ctx (before=%d after=%d result_tag=%d)",
            (int)before,
            (int)after,
            r.value != null ? (int)r.value.tag : -1
        );
        (*fail)++;
    }
}
