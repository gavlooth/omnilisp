module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_jit_policy_closure_method_iterator_boundary_test(Interp* interp, int* pass, int* fail) {
    // Invariant: typed method wrappers that return iterators must detach a thunk
    // payload from the callee TEMP lane before handing the iterator back.
    test_eq(
        interp,
        "jit closure method iterator wrapper detaches thunk boundary",
        "(block (define (wrap (^Closure thunk)) (Iterator thunk)) (define (wrap (^Array arr)) (let (len (length arr)) (let make-it (i 0) (if (= i len) (wrap (lambda () nil)) (wrap (lambda () (cons (ref arr i) (make-it (+ i 1))))))))) (car (next (wrap [1 2 3]))))",
        1,
        pass,
        fail
    );
}

fn void run_jit_policy_rec_closure_type_sig_copy_failure_test(Interp* interp, int* pass, int* fail) {
    // Invariant: detached recursive closure publication fails closed if
    // type-signature cloning into the detached env scope fails.
    main::ScopeRegion* saved_scope = interp.current_scope;
    main::ScopeRegion* source = main::scope_create(saved_scope);
    if (source == null) {
        io::printn("[FAIL] jit policy: rec closure type signature copy failure returns null");
        (*fail)++;
        return;
    }

    SymbolId rec_sym = interp.symbols.intern("__jit_policy_rec_type_sig");
    Value* placeholder = null;
    main::ScopeRegion* detached_env_scope = null;
    Value* closure = null;
    MethodSignature* sig = null;

    interp.current_scope = source;
    Env* captured_env = make_env(interp, interp.global_env);
    if (captured_env != null) {
        placeholder = make_nil(interp);
        env_define_with_barrier(captured_env, rec_sym, placeholder, interp);
        closure = make_env_scope_closure_for_test(interp, captured_env, &detached_env_scope);
    }
    if (closure != null) {
        sig = (MethodSignature*)source.alloc(MethodSignature.sizeof);
        if (sig != null && method_signature_alloc_arrays_in_scope(sig, source, 1, 0)) {
            sig.param_types[0] = interp.tid_Integer;
            sig.param_type_vars[0] = (SymbolId)0;
            sig.has_val_literal[0] = false;
            sig.val_literals[0].tag = NIL;
            sig.val_literals[0].int_value = 0;
            sig.val_literals[0].sym_value = (SymbolId)0;
            closure.closure_val.has_typed_params = true;
            closure.closure_val.type_sig = sig;
        } else {
            sig = null;
        }
    }

    Value* binding_before = closure != null && closure.closure_val.env != null
        ? closure.closure_val.env.lookup(rec_sym)
        : null;
    g_method_signature_scope_copy_call_count = 0;
    g_method_signature_force_scope_copy_fail_on_call = 1;
    Value* published = closure != null ? jit_patch_rec_closure(closure, null, rec_sym, interp) : null;
    g_method_signature_force_scope_copy_fail_on_call = 0;
    Value* binding_after = closure != null && closure.closure_val.env != null
        ? closure.closure_val.env.lookup(rec_sym)
        : null;

    bool setup_ok = captured_env != null &&
                    closure != null &&
                    closure.tag == CLOSURE &&
                    closure.closure_val != null &&
                    detached_env_scope != null &&
                    closure.closure_val.env_scope == detached_env_scope &&
                    sig != null &&
                    closure.closure_val.type_sig == sig &&
                    binding_before != null &&
                    binding_before.tag == NIL;
    bool fail_closed_ok = published == null &&
                          g_method_signature_scope_copy_call_count == 1 &&
                          binding_after == binding_before &&
                          binding_after != null &&
                          binding_after.tag == NIL &&
                          closure.closure_val.type_sig == sig;
    int scope_copy_calls = (int)g_method_signature_scope_copy_call_count;
    g_method_signature_scope_copy_call_count = 0;

    interp.current_scope = saved_scope;
    main::scope_release(source);

    bool ok = setup_ok && fail_closed_ok;
    if (ok) {
        emit_pass_literal("[PASS] jit policy: rec closure type signature copy failure returns null");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: rec closure type signature copy failure returns null (setup_ok=%s fail_closed_ok=%s call_count=%d published=%s binding_after=%s)",
            setup_ok ? "yes" : "no",
            fail_closed_ok ? "yes" : "no",
            scope_copy_calls,
            published == null ? "null" : "non-null",
            binding_after == null ? "null" : (binding_after.tag == NIL ? "nil" : "non-nil")
        );
        (*fail)++;
    }
}
fn void run_jit_policy_stdlib_teardown_redefinition_test(Interp* interp, int* pass, int* fail) {
    // Regression: stdlib-loaded typed method redefinition must still tear down
    // cleanly after interpreter destroy.
    (void)interp;

    Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
    if (tmp == null) {
        io::printn("[FAIL] jit policy: stdlib teardown redefinition destroys cleanly (alloc)");
        (*fail)++;
        return;
    }

    tmp.init();
    if (!bootstrap_runtime_interp(tmp)) {
        io::printn("[FAIL] jit policy: stdlib teardown redefinition destroys cleanly (bootstrap)");
        (*fail)++;
        tmp.destroy();
        mem::free(tmp);
        return;
    }

    EvalResult r = run(
        "(block "
        "  (define (dispatch-teardown (^Integer x)) x) "
        "  (define (dispatch-teardown x) x) "
        "  (define (dispatch-teardown (^Integer x)) (+ x 1)) "
        "  (dispatch-teardown 1))",
        tmp
    );

    bool ok = !r.error.has_error &&
              r.value != null &&
              r.value.tag == INT &&
              r.value.int_val == 2;

    tmp.destroy();
    mem::free(tmp);

    if (ok) {
        emit_pass_literal("[PASS] jit policy: stdlib teardown redefinition destroys cleanly");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: stdlib teardown redefinition destroys cleanly");
        (*fail)++;
    }
}

fn void run_jit_policy_mutable_local_null_box_helpers_fail_closed_test(Interp* interp, int* pass, int* fail) {
    // Invariant: generated mutable-local helper paths must not dereference a
    // null env box if root-box setup failed before later lookup/capture code.
    Value* lookup_err = jit_env_lookup_local(
        interp,
        (uint)interp.symbols.intern("__jit_policy_missing_mutable_box"),
        null
    );
    Env* reparented = jit_env_reparent(null, interp.global_env);

    bool lookup_ok = lookup_err != null &&
                     lookup_err.tag == ERROR &&
                     lookup_err.str_chars != null &&
                     jit_policy_str_contains(
                         lookup_err.str_chars[:lookup_err.str_len],
                         "jit: missing mutable local binding"
                     );
    bool reparent_ok = reparented == interp.global_env;
    bool ok = lookup_ok && reparent_ok;

    if (ok) {
        emit_pass_literal("[PASS] jit policy: mutable local null box helpers fail closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: mutable local null box helpers fail closed (lookup_ok=%s reparent_ok=%s)",
            lookup_ok ? "yes" : "no",
            reparent_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void jit_policy_resuspending_target_entry(void* arg) {
    (void)arg;
    while (true) {
        main::stack_ctx_suspend();
    }
}

fn void jit_policy_complete_after_resume_entry(void* arg) {
    int* step = (int*)arg;
    if (step != null) *step = 1;
    main::stack_ctx_suspend();
    if (step != null) *step = 2;
}

struct JitPolicyBlockedFiberDriveState {
    Interp* interp;
    main::StackCtx* target;
    JitBlockedFiberDriveStatus status;
}

fn void jit_policy_blocked_fiber_drive_entry(void* arg) {
    JitPolicyBlockedFiberDriveState* state = (JitPolicyBlockedFiberDriveState*)arg;
    main::StackContext parent_ctx;
    state.status = jit_continuation_drive_blocked_fiber(state.target, state.interp, &parent_ctx);
}

fn void run_jit_policy_blocked_fiber_drive_limit_test(Interp* interp, int* pass, int* fail) {
    bool saved_running = g_scheduler.running;
    usz saved_fiber_count = g_scheduler.fiber_count;
    usz saved_current_fiber_id = g_scheduler.current_fiber_id;
    FiberEntry saved_fiber_0 = g_scheduler.fibers[0];
    usz saved_limit = g_jit_blocked_fiber_drive_limit_for_test;

    main::StackCtx* target = main::stack_ctx_create(&interp.stack_ctx_pool);
    main::StackCtx* outer = main::stack_ctx_create(&interp.stack_ctx_pool);
    JitPolicyBlockedFiberDriveState state = {
        .interp = interp,
        .target = target,
        .status = JIT_BLOCKED_FIBER_DRIVE_OK,
    };

    bool setup_ok = target != null && outer != null;
    if (setup_ok) {
        main::stack_ctx_init(target, &jit_policy_resuspending_target_entry, null);
        main::StackContext target_parent_ctx;
        main::stack_ctx_switch_to(target, &target_parent_ctx);
        setup_ok = target.status == main::StackCtxStatus.CTX_SUSPENDED;
    }

    int resume_count = 0;
    if (setup_ok) {
        g_scheduler.running = true;
        g_scheduler.fiber_count = 1;
        g_scheduler.current_fiber_id = 0;
        g_scheduler.fibers[0] = {};
        g_scheduler.fibers[0].state = FIBER_BLOCKED;
        g_scheduler.fibers[0].owner_interp = interp;
        g_jit_blocked_fiber_drive_limit_for_test = 2;

        main::stack_ctx_init(outer, &jit_policy_blocked_fiber_drive_entry, &state);
        main::StackContext outer_parent_ctx;
        main::stack_ctx_switch_to(outer, &outer_parent_ctx);
        while (outer.status == main::StackCtxStatus.CTX_SUSPENDED && resume_count < 8) {
            resume_count++;
            main::stack_ctx_resume(outer, &outer_parent_ctx);
        }
    }

    g_jit_blocked_fiber_drive_limit_for_test = saved_limit;
    g_scheduler.fibers[0] = saved_fiber_0;
    g_scheduler.current_fiber_id = saved_current_fiber_id;
    g_scheduler.fiber_count = saved_fiber_count;
    g_scheduler.running = saved_running;

    bool ok = setup_ok &&
              state.status == JIT_BLOCKED_FIBER_DRIVE_LIMIT_EXCEEDED &&
              outer.status == main::StackCtxStatus.CTX_COMPLETED &&
              resume_count <= 3;
    int outer_status = outer != null ? (int)outer.status : -1;
    if (outer != null) main::stack_ctx_destroy(outer, &interp.stack_ctx_pool);
    if (target != null) main::stack_ctx_destroy(target, &interp.stack_ctx_pool);

    if (ok) {
        emit_pass_literal("[PASS] jit policy: blocked fiber drive limit fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: blocked fiber drive limit fails closed (setup_ok=%s status=%d outer_status=%d resumes=%d)",
            setup_ok ? "true" : "false",
            (int)state.status,
            outer_status,
            resume_count
        );
        (*fail)++;
    }
}

fn void run_jit_policy_multishot_handle_continuation_preserves_retained_state_test(Interp* interp, int* pass, int* fail) {
    main::StackCtx* source = main::stack_ctx_create(&interp.stack_ctx_pool);
    HandleEffectState* state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
    Continuation* k = interp.alloc_lisp_continuation();
    Value* k_val = null;
    int step = 0;
    bool setup_ok = source != null && state != null && k != null;

    if (setup_ok) {
        main::stack_ctx_init(source, &jit_policy_complete_after_resume_entry, &step);
        main::StackContext parent_ctx;
        main::stack_ctx_switch_to(source, &parent_ctx);
        setup_ok = source.status == main::StackCtxStatus.CTX_SUSPENDED && step == 1;
    }

    if (setup_ok) {
        handle_effect_state_init(state, interp);
        state.continuation_refcount = 1;
        k.ctx = source;
        k.owner_interp = interp;
        k.is_coroutine_based = true;
        k.handle_state = state;
        k.handle_retained = true;
        k_val = make_continuation(interp, k);
        setup_ok = k_val != null && k_val.tag == CONTINUATION;
    }

    Value* result = null;
    if (setup_ok) {
        result = jit_apply_handle_continuation_value(interp, k_val, make_int(interp, 7));
    }

    int refcount_after_resume = state != null ? (int)state.continuation_refcount : -1;
    bool retained_ok = setup_ok &&
        step == 2 &&
        k.handle_retained &&
        refcount_after_resume == 1;

    if (k != null) {
        k.ctx = null;
        k.handle_state = null;
        k.handle_retained = false;
    }
    if (source != null) main::stack_ctx_destroy(source, &interp.stack_ctx_pool);
    if (state != null) {
        state.frame_active = false;
        if (handle_effect_state_is_live(state)) handle_effect_state_release(state);
    }

    if (retained_ok) {
        emit_pass_literal("[PASS] jit policy: multi-shot handle continuation preserves retained state");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: multi-shot handle continuation preserves retained state (setup_ok=%s step=%d retained=%s refcount=%d result=%s)",
            setup_ok ? "yes" : "no",
            step,
            k != null && k.handle_retained ? "yes" : "no",
            refcount_after_resume,
            result == null ? "null" : "value"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_handle_run_signals_backedge_interrupt_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    Interp other = {};
    other.init();
    defer other.destroy();

    bool ok = register_primitives(&other);
    HandleEffectState* state = null;
    Value* result = null;
    Continuation* saved_head = null;
    bool state_initialized = false;

    if (ok) {
        state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
        ok = state != null;
    }
    if (ok) {
        handle_effect_state_init(state, &other);
        state_initialized = true;
        state.handler_copy.clause_count = 1;
        state.handler_copy.tags = (SymbolId*)mem::malloc(SymbolId.sizeof);
        state.handler_copy.closures = (Value**)mem::malloc(Value*.sizeof);
        ok = state.handler_copy.tags != null && state.handler_copy.closures != null;
    }
    if (ok) {
        EvalResult closure_r = run("(lambda (k x) x)", &other);
        ok = !closure_r.error.has_error && closure_r.value != null && closure_r.value.tag == CLOSURE;
        if (ok) {
            SymbolId signal_tag = other.symbols.intern("ask");
            state.signal_tag = signal_tag;
            state.signal_arg = make_int(&other, 0);
            state.handler_copy.tags[0] = signal_tag;
            state.handler_copy.closures[0] = closure_r.value;
            state.signaled = true;
            saved_head = other.continuation_head;

            interp_interrupt_clear(&other);
            interp_sigint_bind(&other);
            interp_sigint_debug_mark_pending();
            result = jit_handle_run_signals(&other, null, state, 0);
            interp_sigint_unbind(&other);
            interp_interrupt_clear(&other);
        }
    }

    bool error_ok = result != null &&
        result.tag == ERROR &&
        result.str_chars != null &&
        jit_policy_str_contains(result.str_chars[:result.str_len], "evaluation interrupted");
    bool no_continuation_alloc = other.continuation_head == saved_head;
    bool signal_preserved = state != null && state.signaled;
    bool final_ok = ok && error_ok && no_continuation_alloc && signal_preserved;

    if (state_initialized) {
        handle_effect_state_release(state);
    } else if (state != null) {
        mem::free(state);
    }

    if (final_ok) {
        emit_pass_literal("[PASS] jit policy: handle run signals backedge honors interrupt");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: handle run signals backedge honors interrupt (setup_ok=%s error_ok=%s no_continuation=%s signal_preserved=%s)",
            ok ? "true" : "false",
            error_ok ? "true" : "false",
            no_continuation_alloc ? "true" : "false",
            signal_preserved ? "true" : "false"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_handle_dispatch_signals_backedge_interrupt_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    Interp other = {};
    other.init();
    defer other.destroy();

    HandleEffectState* state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
    bool ok = state != null;
    EvalResult out_result = {};
    bool dispatched = false;
    Continuation* saved_head = null;

    if (ok) {
        handle_effect_state_init(state, &other);
        state.signaled = true;
        state.signal_tag = other.symbols.intern("ask");
        state.signal_arg = make_int(&other, 0);
        saved_head = other.continuation_head;

        interp_interrupt_clear(&other);
        interp_sigint_bind(&other);
        interp_sigint_debug_mark_pending();
        dispatched = jit_handle_dispatch_signals(state, null, null, &other, null, &out_result);
        interp_sigint_unbind(&other);
        interp_interrupt_clear(&other);
    }

    bool error_ok = dispatched &&
        out_result.error.has_error &&
        jit_policy_str_contains(out_result.error.message[:256], "evaluation interrupted");
    bool no_continuation_alloc = other.continuation_head == saved_head;
    bool signal_preserved = state != null && state.signaled;
    bool final_ok = ok && error_ok && no_continuation_alloc && signal_preserved;

    if (state != null) handle_effect_state_release(state);

    if (final_ok) {
        emit_pass_literal("[PASS] jit policy: handle dispatch signals backedge honors interrupt");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: handle dispatch signals backedge honors interrupt (setup_ok=%s dispatched=%s error_ok=%s no_continuation=%s signal_preserved=%s)",
            ok ? "true" : "false",
            dispatched ? "true" : "false",
            error_ok ? "true" : "false",
            no_continuation_alloc ? "true" : "false",
            signal_preserved ? "true" : "false"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_match_binding_growth_failure_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: match binding growth failure skipped when jit disabled");
        (*pass)++;
        return;
    }

    char[] source = "(match '(1 2 3 4 5) ([a b c d e] (+ a e)) (_ 0))";
    g_env_force_bindings_alloc_oom = true;
    JitTopLevelEval jit_eval = eval_jit_top_level(interp, source);
    g_env_force_bindings_alloc_oom = false;

    Value* result = jit_eval.value;
    bool ok = jit_eval.ok &&
              result != null &&
              result.tag == ERROR &&
              jit_policy_str_contains(result.str_chars[:result.str_len], "out of memory while binding match clause");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: match binding growth failure returns error");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: match binding growth failure returns error");
        print_jit_failure_debug(interp, "jit match binding growth failure", jit_eval.stage, result);
        (*fail)++;
    }
}

fn void run_jit_policy_multi_arg_binding_growth_failure_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: multi-arg binding growth failure skipped when jit disabled");
        (*pass)++;
        return;
    }

    char[] source = "((lambda (a b c d e) (+ a e)) 1 2 3 4 5)";
    g_env_force_bindings_alloc_oom = true;
    JitTopLevelEval jit_eval = eval_jit_top_level(interp, source);
    g_env_force_bindings_alloc_oom = false;

    Value* result = jit_eval.value;
    bool ok = jit_eval.ok &&
              result != null &&
              result.tag == ERROR &&
              jit_policy_str_contains(result.str_chars[:result.str_len], "out of memory while binding argument");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: multi-arg binding growth failure returns error");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: multi-arg binding growth failure returns error");
        print_jit_failure_debug(interp, "jit multi-arg binding growth failure", jit_eval.stage, result);
        (*fail)++;
    }
}

fn void run_jit_policy_local_env_capture_growth_failure_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: local env capture growth failure skipped when jit disabled");
        (*pass)++;
        return;
    }

    char[] source = "(let (x 1) (lambda () x))";
    g_env_force_bindings_alloc_oom = true;
    JitTopLevelEval jit_eval = eval_jit_top_level(interp, source);
    g_env_force_bindings_alloc_oom = false;

    Value* result = jit_eval.value;
    bool ok = jit_eval.ok &&
              result != null &&
              result.tag == ERROR &&
              jit_policy_str_contains(result.str_chars[:result.str_len], "out of memory while binding local");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: local env capture growth failure returns error");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: local env capture growth failure returns error");
        print_jit_failure_debug(interp, "jit local env capture growth failure", jit_eval.stage, result);
        (*fail)++;
    }
}

fn void run_jit_policy_lambda_null_body_fails_closed_test(Interp* interp, int* pass, int* fail) {
    Expr zero_arg = {};
    ExprLambda zero_lambda = {};
    zero_arg.tag = E_LAMBDA;
    zero_arg.lambda = &zero_lambda;
    zero_lambda.param = INVALID_SYMBOL_ID;
    zero_lambda.has_rest = false;
    zero_lambda.body = null;

    Expr one_arg = {};
    ExprLambda one_lambda = {};
    one_arg.tag = E_LAMBDA;
    one_arg.lambda = &one_lambda;
    one_lambda.param = interp.symbols.intern("__jit_policy_lambda_null_body_arg");
    one_lambda.has_rest = false;
    one_lambda.body = null;

    Value* zero_result = jit_make_closure_from_expr(interp, &zero_arg, interp.global_env);
    Value* one_result = jit_make_closure_from_expr(interp, &one_arg, interp.global_env);

    bool zero_ok = zero_result != null &&
                   zero_result.tag == ERROR &&
                   jit_policy_str_contains(zero_result.str_chars[:zero_result.str_len], "malformed lambda expression");
    bool one_ok = one_result != null &&
                  one_result.tag == ERROR &&
                  jit_policy_str_contains(one_result.str_chars[:one_result.str_len], "malformed lambda expression");
    bool ok = zero_ok && one_ok;

    if (ok) {
        emit_pass_literal("[PASS] jit policy: lambda null body fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: lambda null body fails closed (zero_ok=%s one_ok=%s)",
            zero_ok ? "yes" : "no",
            one_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_fast_path_invalid_primitive_entry_fails_closed_test(Interp* interp, int* pass, int* fail) {
    SymbolId tag = interp.symbols.intern("__jit-policy-fast-path-invalid");
    Value* arg = make_int(interp, 1);
    Value* invalid = interp.alloc_value();
    usz saved_count = interp.fast_path_count;
    bool setup_ok = tag != INVALID_SYMBOL_ID &&
        arg != null &&
        invalid != null &&
        saved_count < interp.fast_path_table.len;
    if (setup_ok) {
        *invalid = {};
        invalid.tag = PRIMITIVE;
        invalid.prim_val = null;
        interp.fast_path_table[saved_count].tag = tag;
        interp.fast_path_table[saved_count].prim = invalid;
        interp.fast_path_count = saved_count + 1;
    }

    bool handled = false;
    Value* dispatch_result = setup_ok
        ? jit_signal_try_fast_path_dispatch(interp, tag, arg, &handled)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* legacy_result = setup_ok
        ? jit_signal_try_fast_path(tag, arg, interp)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");

    if (setup_ok) {
        interp.fast_path_count = saved_count;
        interp.fast_path_table[saved_count].tag = INVALID_SYMBOL_ID;
        interp.fast_path_table[saved_count].prim = null;
    }

    bool ok = setup_ok &&
        handled &&
        dispatch_result != null &&
        dispatch_result.tag == ERROR &&
        legacy_result != null &&
        legacy_result.tag == ERROR &&
        jit_policy_str_contains(dispatch_result.str_chars[:dispatch_result.str_len], "invalid primitive entry") &&
        jit_policy_str_contains(legacy_result.str_chars[:legacy_result.str_len], "invalid primitive entry");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: fast-path invalid primitive entry fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: fast-path invalid primitive entry fails closed (setup_ok=%s handled=%s)",
            setup_ok ? "yes" : "no",
            handled ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_multi_arg_invalid_primitive_state_fails_closed_test(Interp* interp, int* pass, int* fail) {
    Value* invalid = interp.alloc_value();
    Value* arg = make_int(interp, 11);
    Value* tail = make_nil(interp);
    Value* arg_list = arg != null && tail != null ? make_cons(interp, arg, tail) : null;
    bool setup_ok = invalid != null && arg_list != null && arg_list.tag == CONS;
    if (setup_ok) {
        *invalid = {};
        invalid.tag = PRIMITIVE;
        invalid.prim_val = null;
    }

    Value* direct_result = setup_ok
        ? jit_apply_multi_args_primitive(interp, invalid, arg_list, 1)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* routed_result = setup_ok
        ? jit_apply_multi_args(interp, invalid, arg_list, 1)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* tail_result = setup_ok
        ? jit_apply_multi_args_tail(interp, invalid, arg_list, 1)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");

    bool ok = setup_ok &&
        direct_result != null &&
        direct_result.tag == ERROR &&
        routed_result != null &&
        routed_result.tag == ERROR &&
        tail_result != null &&
        tail_result.tag == ERROR &&
        jit_policy_str_contains(direct_result.str_chars[:direct_result.str_len], "invalid primitive application state") &&
        jit_policy_str_contains(routed_result.str_chars[:routed_result.str_len], "invalid primitive application state") &&
        jit_policy_str_contains(tail_result.str_chars[:tail_result.str_len], "invalid primitive application state");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: multi-arg invalid primitive state fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: multi-arg invalid primitive state fails closed (setup_ok=%s direct=%d routed=%d tail=%d)",
            setup_ok ? "yes" : "no",
            direct_result != null ? (int)direct_result.tag : -1,
            routed_result != null ? (int)routed_result.tag : -1,
            tail_result != null ? (int)tail_result.tag : -1
        );
        (*fail)++;
    }
}

fn bool jit_policy_invalid_method_table_error_ok(Value* result) {
    return result != null &&
           result.tag == ERROR &&
           jit_policy_str_contains(result.str_chars[:result.str_len], "invalid method table");
}

fn void run_jit_policy_invalid_method_table_state_fails_closed_test(Interp* interp, int* pass, int* fail) {
    Value* invalid = interp.alloc_value();
    Value* first_arg = make_int(interp, 7);
    Value* arg_tail = make_nil(interp);
    Value* arg_list = first_arg != null && arg_tail != null
        ? make_cons(interp, first_arg, arg_tail)
        : null;
    Value* fallback_value = make_int(interp, 42);
    SymbolId define_sym = interp.symbols.intern("__jit_policy_invalid_method_table_binding");

    bool setup_ok = invalid != null &&
                    first_arg != null &&
                    arg_list != null &&
                    arg_list.tag == CONS &&
                    fallback_value != null &&
                    define_sym != INVALID_SYMBOL_ID;
    if (setup_ok) {
        *invalid = {};
        invalid.tag = METHOD_TABLE;
        invalid.method_table_val = null;
        setup_ok = interp.global_env.define(define_sym, invalid);
    }

    Value*[1] args = { first_arg };
    Value* direct_match = setup_ok
        ? find_best_method(null, args[:1], interp)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* direct_format = setup_ok
        ? format_dispatch_error(null, args[:1], interp)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* single_apply = setup_ok
        ? jit_apply_value(invalid, first_arg, interp)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* multi_apply = setup_ok
        ? jit_apply_multi_args(interp, invalid, arg_list, 1)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* tail_apply = setup_ok
        ? jit_apply_multi_args_tail(interp, invalid, arg_list, 1)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* define_result = setup_ok
        ? jit_eval_define(interp, define_sym, fallback_value)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");

    bool ok = setup_ok &&
              jit_policy_invalid_method_table_error_ok(direct_match) &&
              jit_policy_invalid_method_table_error_ok(direct_format) &&
              jit_policy_invalid_method_table_error_ok(single_apply) &&
              jit_policy_invalid_method_table_error_ok(multi_apply) &&
              jit_policy_invalid_method_table_error_ok(tail_apply) &&
              jit_policy_invalid_method_table_error_ok(define_result);

    if (ok) {
        emit_pass_literal("[PASS] jit policy: invalid method table state fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: invalid method table state fails closed (setup_ok=%s direct=%d format=%d single=%d multi=%d tail=%d define=%d)",
            setup_ok ? "yes" : "no",
            direct_match != null ? (int)direct_match.tag : -1,
            direct_format != null ? (int)direct_format.tag : -1,
            single_apply != null ? (int)single_apply.tag : -1,
            multi_apply != null ? (int)multi_apply.tag : -1,
            tail_apply != null ? (int)tail_apply.tag : -1,
            define_result != null ? (int)define_result.tag : -1
        );
        (*fail)++;
    }
}

fn bool jit_policy_invalid_closure_error_ok(Value* result) {
    return result != null &&
           result.tag == ERROR &&
           jit_policy_str_contains(result.str_chars[:result.str_len], "invalid closure state");
}

fn void run_jit_policy_invalid_closure_state_fails_closed_test(Interp* interp, int* pass, int* fail) {
    Value* arg = make_int(interp, 13);
    Value* tail = make_nil(interp);
    Value* arg_list = arg != null && tail != null
        ? make_cons(interp, arg, tail)
        : null;
    Value* null_payload = interp.alloc_value();
    Value* null_body_value = interp.alloc_value();
    Closure null_body = {};
    null_body.env = interp.global_env;
    null_body.body = null;

    bool setup_ok = arg != null &&
                    arg_list != null &&
                    arg_list.tag == CONS &&
                    null_payload != null &&
                    null_body_value != null;
    if (setup_ok) {
        *null_payload = {};
        null_payload.tag = CLOSURE;
        null_payload.closure_val = null;

        *null_body_value = {};
        null_body_value.tag = CLOSURE;
        null_body_value.closure_val = &null_body;
    }

    Value* single_null_payload = setup_ok
        ? jit_apply_value(null_payload, arg, interp)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* tail_null_payload = setup_ok
        ? jit_apply_value_tail(null_payload, arg, interp)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* multi_null_payload = setup_ok
        ? jit_apply_multi_args(interp, null_payload, arg_list, 1)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* multi_tail_null_payload = setup_ok
        ? jit_apply_multi_args_tail(interp, null_payload, arg_list, 1)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");

    Value* single_null_body = setup_ok
        ? jit_apply_value(null_body_value, arg, interp)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* tail_null_body = setup_ok
        ? jit_apply_value_tail(null_body_value, arg, interp)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* multi_null_body = setup_ok
        ? jit_apply_multi_args(interp, null_body_value, arg_list, 1)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* multi_tail_null_body = setup_ok
        ? jit_apply_multi_args_tail(interp, null_body_value, arg_list, 1)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");

    Value* direct_zero = setup_ok
        ? jit_apply_multi_args_zeroarg_closure(interp, null_body_value)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* direct_variadic = setup_ok
        ? jit_apply_multi_args_variadic_closure(interp, null_body_value, arg_list, 1)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* direct_multi = setup_ok
        ? jit_apply_multi_args_closure_multi(interp, null_body_value, arg_list, 1)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* direct_tail_variadic = setup_ok
        ? jit_apply_multi_args_tail_variadic(interp, null_body_value, arg_list, 1)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
    Value* direct_tail_multi = setup_ok
        ? jit_apply_multi_args_tail_closure_multi(interp, null_body_value, arg_list, 1)
        : runtime_raise(interp, "runtime/evaluation-error", "setup failed");

    BoundaryEnvCopyFault null_payload_fault = BOUNDARY_ENV_COPY_FAULT_NONE;
    BoundaryEnvCopyFault null_body_fault = BOUNDARY_ENV_COPY_FAULT_NONE;
    bool null_payload_copy = setup_ok
        ? jit_copy_closure_env_if_needed(interp, null_payload, interp.global_env, false, &null_payload_fault)
        : true;
    bool null_body_copy = setup_ok
        ? jit_copy_closure_env_if_needed(interp, null_body_value, interp.global_env, false, &null_body_fault)
        : false;

    bool ok = setup_ok &&
              jit_policy_invalid_closure_error_ok(single_null_payload) &&
              jit_policy_invalid_closure_error_ok(tail_null_payload) &&
              jit_policy_invalid_closure_error_ok(multi_null_payload) &&
              jit_policy_invalid_closure_error_ok(multi_tail_null_payload) &&
              jit_policy_invalid_closure_error_ok(single_null_body) &&
              jit_policy_invalid_closure_error_ok(tail_null_body) &&
              jit_policy_invalid_closure_error_ok(multi_null_body) &&
              jit_policy_invalid_closure_error_ok(multi_tail_null_body) &&
              jit_policy_invalid_closure_error_ok(direct_zero) &&
              jit_policy_invalid_closure_error_ok(direct_variadic) &&
              jit_policy_invalid_closure_error_ok(direct_multi) &&
              jit_policy_invalid_closure_error_ok(direct_tail_variadic) &&
              jit_policy_invalid_closure_error_ok(direct_tail_multi) &&
              !null_payload_copy &&
              null_body_copy &&
              null_payload_fault == BOUNDARY_ENV_COPY_FAULT_INVALID_CLOSURE_STATE &&
              null_body_fault == BOUNDARY_ENV_COPY_FAULT_NONE;

    if (ok) {
        emit_pass_literal("[PASS] jit policy: invalid closure state fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: invalid closure state fails closed (setup_ok=%s null_payload_copy=%s null_body_copy=%s faults=%s/%s single=%d tail=%d multi=%d multi_tail=%d body_single=%d body_tail=%d body_multi=%d body_multi_tail=%d)",
            setup_ok ? "yes" : "no",
            null_payload_copy ? "yes" : "no",
            null_body_copy ? "yes" : "no",
            (ZString)boundary_env_copy_fault_name(null_payload_fault),
            (ZString)boundary_env_copy_fault_name(null_body_fault),
            single_null_payload != null ? (int)single_null_payload.tag : -1,
            tail_null_payload != null ? (int)tail_null_payload.tag : -1,
            multi_null_payload != null ? (int)multi_null_payload.tag : -1,
            multi_tail_null_payload != null ? (int)multi_tail_null_payload.tag : -1,
            single_null_body != null ? (int)single_null_body.tag : -1,
            tail_null_body != null ? (int)tail_null_body.tag : -1,
            multi_null_body != null ? (int)multi_null_body.tag : -1,
            multi_tail_null_body != null ? (int)multi_tail_null_body.tag : -1
        );
        (*fail)++;
    }
}
