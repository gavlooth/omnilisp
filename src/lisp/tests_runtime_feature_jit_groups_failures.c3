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

fn void run_jit_policy_multi_arg_primitive_buffer_failure_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: multi-arg primitive buffer failure skipped when jit disabled");
        (*pass)++;
        return;
    }

    char[] source = "(+ 1 2 3)";
    g_jit_force_multi_arg_buffer_alloc_fail = true;
    JitTopLevelEval jit_eval = eval_jit_top_level(interp, source);
    g_jit_force_multi_arg_buffer_alloc_fail = false;

    Value* result = jit_eval.value;
    bool ok = jit_eval.ok &&
              result != null &&
              result.tag == ERROR &&
              jit_policy_str_contains(result.str_chars[:result.str_len], "out of memory while preparing call arguments");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: multi-arg primitive buffer failure returns error");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: multi-arg primitive buffer failure returns error");
        print_jit_failure_debug(interp, "jit multi-arg primitive buffer failure", jit_eval.stage, result);
        (*fail)++;
    }
}

fn void run_jit_policy_multi_arg_method_table_buffer_failure_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: multi-arg method-table buffer failure skipped when jit disabled");
        (*pass)++;
        return;
    }

    char[] source =
        "(block "
        "  (define (dispatch (^Integer a) (^Integer b)) (+ a b)) "
        "  (dispatch 20 22))";
    g_jit_force_multi_arg_buffer_alloc_fail = true;
    JitTopLevelEval jit_eval = eval_jit_top_level(interp, source);
    g_jit_force_multi_arg_buffer_alloc_fail = false;

    Value* result = jit_eval.value;
    bool ok = jit_eval.ok &&
              result != null &&
              result.tag == ERROR &&
              jit_policy_str_contains(result.str_chars[:result.str_len], "out of memory while preparing call arguments");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: multi-arg method-table buffer failure returns error");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: multi-arg method-table buffer failure returns error");
        print_jit_failure_debug(interp, "jit multi-arg method-table buffer failure", jit_eval.stage, result);
        (*fail)++;
    }
}

fn void run_jit_policy_match_guard_binding_growth_failure_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: match guard binding growth failure skipped when jit disabled");
        (*pass)++;
        return;
    }

    char[] source = "(match [1 2 3 4 5] ((? true [a b c d e]) (+ a e)) (_ 0))";
    g_env_force_bindings_alloc_oom = true;
    JitTopLevelEval jit_eval = eval_jit_top_level(interp, source);
    g_env_force_bindings_alloc_oom = false;

    Value* result = jit_eval.value;
    bool ok = jit_eval.ok &&
              result != null &&
              result.tag == ERROR &&
              jit_policy_str_contains(result.str_chars[:result.str_len], "out of memory while defining binding");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: match guard binding growth failure returns error");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: match guard binding growth failure returns error");
        print_jit_failure_debug(interp, "jit match guard binding growth failure", jit_eval.stage, result);
        (*fail)++;
    }
}

fn void run_jit_policy_invalid_primitive_state_fails_closed_test(Interp* interp, int* pass, int* fail) {
    Value* arg = make_int(interp, 9);
    Value* invalid = interp.alloc_value();
    bool setup_ok = arg != null && invalid != null;
    if (setup_ok) {
        *invalid = {};
        invalid.tag = PRIMITIVE;
        invalid.prim_val = null;
    }

    Value* result = setup_ok ? jit_apply_value_primitive(invalid, arg, interp) : raise_error(interp, "setup failed");
    bool ok = setup_ok &&
        result != null &&
        result.tag == ERROR &&
        jit_policy_str_contains(result.str_chars[:result.str_len], "invalid primitive application state");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: invalid primitive state fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: invalid primitive state fails closed (setup_ok=%s tag=%d)",
            setup_ok ? "yes" : "no",
            result != null ? (int)result.tag : -1
        );
        (*fail)++;
    }
}

fn void run_jit_policy_pending_raise_string_alloc_failure_test(Interp* interp, int* pass, int* fail) {
    bool saved_force = g_value_force_make_string_alloc_fail;
    g_value_force_make_string_alloc_fail = true;
    EvalResult r = run("(handle (/ 1 0) (raise msg 42))", interp);
    g_value_force_make_string_alloc_fail = saved_force;

    char[] msg = r.error.message[:256];
    bool ok = r.error.has_error &&
        jit_policy_str_contains(msg, "out of memory while allocating string");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: pending raise string materialization fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: pending raise string materialization fails closed (run_error=%s msg=%s)",
            r.error.has_error ? "yes" : "no",
            r.error.has_error ? (ZString)msg : (ZString)"<none>"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_pending_raise_payload_alloc_failure_test(Interp* interp, int* pass, int* fail) {
    bool saved_force = g_hashmap_force_struct_alloc_fail;
    g_hashmap_force_struct_alloc_fail = true;
    EvalResult r = run("(handle (Dictionary 'a) (raise msg 42))", interp);
    g_hashmap_force_struct_alloc_fail = saved_force;

    char[] msg = r.error.message[:256];
    bool ok = r.error.has_error &&
        jit_policy_str_contains(msg, "raise: failed to construct payload");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: pending raise payload construction fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: pending raise payload construction fails closed (run_error=%s msg=%s)",
            r.error.has_error ? "yes" : "no",
            r.error.has_error ? (ZString)msg : (ZString)"<none>"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_dispatch_payload_alloc_failure_does_not_preseed_raise_test(Interp* interp, int* pass, int* fail) {
    usz saved_handler_count = interp.handler_count;
    bool setup_ok = interp.handler_count < interp.handler_capacity || interp.grow_handler_stack();

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

    Value* data = null;
    bool saved_force = g_hashmap_force_struct_alloc_fail;
    if (setup_ok) {
        interp.flags.raise_pending = false;
        interp.raise_payload = null;
        interp.raise_msg_len = 0;
        interp.handler_stack[interp.handler_count++] = handler;
        g_hashmap_force_struct_alloc_fail = true;
        data = dispatch_make_payload_data(interp, 8);
        g_hashmap_force_struct_alloc_fail = saved_force;
        interp.handler_count = saved_handler_count;
    }
    g_hashmap_force_struct_alloc_fail = saved_force;

    bool ok = setup_ok &&
              data == null &&
              !interp.flags.raise_pending &&
              interp.raise_payload == null &&
              interp.raise_msg_len == 0;

    if (ok) {
        emit_pass_literal("[PASS] jit policy: dispatch payload allocation failure does not preseed raise");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: dispatch payload allocation failure does not preseed raise (setup_ok=%s data=%s pending=%s)",
            setup_ok ? "yes" : "no",
            data == null ? "null" : "non-null",
            interp.flags.raise_pending ? "yes" : "no"
        );
        interp.handler_count = saved_handler_count;
        interp.flags.raise_pending = false;
        interp.raise_payload = null;
        interp.raise_msg_len = 0;
        (*fail)++;
    }
}

fn void run_jit_policy_handle_continuation_alloc_failure_test(Interp* interp, int* pass, int* fail) {
    bool saved_force = g_value_force_alloc_lisp_continuation_fail;
    g_value_force_alloc_lisp_continuation_fail = true;
    EvalResult r = run("(handle (+ 1 (signal ask 0)) (ask x x))", interp);
    g_value_force_alloc_lisp_continuation_fail = saved_force;

    char[] msg = r.error.message[:256];
    bool ok = r.error.has_error &&
        jit_policy_str_contains(msg, "handle: failed to allocate continuation");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: handled effect continuation allocation fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: handled effect continuation allocation fails closed (run_error=%s msg=%s)",
            r.error.has_error ? "yes" : "no",
            r.error.has_error ? (ZString)msg : (ZString)"<none>"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_handler_arg_list_alloc_failure_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    Interp* other = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = other != null;
    if (ok) other.init();

    main::StackCtx* ctx = null;
    HandleEffectState* state = null;
    Value* handler_closure = null;
    Value* signal_result = null;
    Value* raise_result = null;
    if (ok) {
        EvalResult closure_r = run("(lambda (k x) x)", other);
        ok = !closure_r.error.has_error && closure_r.value != null && closure_r.value.tag == CLOSURE;
        if (ok) handler_closure = closure_r.value;
    }
    if (ok) {
        state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
        ok = state != null;
    }
    if (ok) {
        handle_effect_state_init(state, other);
        state.handler_copy.clause_count = 1;
        state.handler_copy.tags = (SymbolId*)mem::malloc(SymbolId.sizeof);
        state.handler_copy.closures = (Value**)mem::malloc(Value*.sizeof);
        ok = state.handler_copy.tags != null && state.handler_copy.closures != null;
    }
    if (ok) {
        ctx = main::stack_ctx_create(&other.stack_ctx_pool);
        ok = ctx != null;
    }
    if (ok) {
        int saved_force = g_value_force_make_list2_alloc_fail_nth;

        state.signal_tag = other.symbols.intern("ask");
        state.signal_arg = make_int(other, 0);
        state.handler_copy.tags[0] = state.signal_tag;
        state.handler_copy.closures[0] = handler_closure;
        state.signaled = true;
        g_value_force_make_list2_alloc_fail_nth = 1;
        signal_result = jit_handle_run_signals(other, ctx, state, 0);

        other.flags.raise_pending = true;
        other.raise_payload = make_int(other, 42);
        other.raise_msg_len = 0;
        other.raise_msg[0] = 0;
        state.handler_copy.tags[0] = other.sym_raise;
        g_value_force_make_list2_alloc_fail_nth = 1;
        raise_result = jit_handle_dispatch_pending_raise_value(other, state);

        g_value_force_make_list2_alloc_fail_nth = saved_force;
    }

    ok = ok &&
        signal_result != null &&
        signal_result.tag == ERROR &&
        jit_policy_str_contains(signal_result.str_chars[:signal_result.str_len], "handle: failed to construct handler args") &&
        raise_result != null &&
        raise_result.tag == ERROR &&
        jit_policy_str_contains(raise_result.str_chars[:raise_result.str_len], "handle: failed to construct raise handler args");

    if (ctx != null) main::stack_ctx_destroy(ctx, &other.stack_ctx_pool);
    if (state != null) handle_effect_state_release(state);
    if (other != null) {
        other.destroy();
        mem::free(other);
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: handler arg list construction fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: handler arg list construction fails closed (signal_tag=%d raise_tag=%d)",
            signal_result != null ? (int)signal_result.tag : -1,
            raise_result != null ? (int)raise_result.tag : -1
        );
        (*fail)++;
    }
}

fn void run_jit_policy_multi_arg_malformed_list_fails_closed_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    Interp* other = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = other != null;
    if (ok) other.init();

    Value* fn_value = null;
    Value* arg_list = null;
    Value* result = null;
    if (ok) {
        EvalResult closure_r = run("(lambda (a) (lambda (b) (+ a b)))", other);
        ok = !closure_r.error.has_error && closure_r.value != null && closure_r.value.tag == CLOSURE;
        if (ok) fn_value = closure_r.value;
    }
    if (ok) {
        arg_list = make_cons(other, make_int(other, 7), make_nil(other));
        ok = arg_list != null && arg_list.tag == CONS;
    }
    if (ok) {
        result = jit_apply_multi_args(other, fn_value, arg_list, 2);
        ok = result != null &&
            result.tag == ERROR &&
            jit_policy_str_contains(result.str_chars[:result.str_len], "arg list too short");
    }

    if (other != null) {
        other.destroy();
        mem::free(other);
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: malformed multi-arg list fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: malformed multi-arg list fails closed (result_tag=%d)",
            result != null ? (int)result.tag : -1
        );
        (*fail)++;
    }
}

fn void run_jit_policy_quasiquote_cons_alloc_failure_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    Interp* other = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = other != null;
    if (ok) other.init();

    Expr* nested_expr = null;
    Expr* list_expr = null;
    Value* nested_result = null;
    Value* list_result = null;
    if (ok) {
        nested_expr = parse("``x", other);
        list_expr = parse("`(a b c)", other);
        ok = nested_expr != null && nested_expr.tag == E_QUASIQUOTE &&
            list_expr != null && list_expr.tag == E_QUASIQUOTE;
    }
    if (ok) {
        int saved_force = g_jit_qq_force_cons_alloc_fail_nth;
        g_jit_qq_force_cons_alloc_fail_nth = 1;
        nested_result = jit_eval_quasiquote(other, nested_expr, other.global_env);
        g_jit_qq_force_cons_alloc_fail_nth = 1;
        list_result = jit_eval_quasiquote(other, list_expr, other.global_env);
        g_jit_qq_force_cons_alloc_fail_nth = saved_force;
        ok = nested_result != null &&
            nested_result.tag == ERROR &&
            jit_policy_str_contains(nested_result.str_chars[:nested_result.str_len], "quasiquote: failed to allocate pair") &&
            list_result != null &&
            list_result.tag == ERROR &&
            jit_policy_str_contains(list_result.str_chars[:list_result.str_len], "quasiquote: failed to allocate pair");
    }

    if (other != null) {
        other.destroy();
        mem::free(other);
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: quasiquote cons allocation failures fail closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: quasiquote cons allocation failures fail closed (nested_tag=%d list_tag=%d)",
            nested_result != null ? (int)nested_result.tag : -1,
            list_result != null ? (int)list_result.tag : -1
        );
        (*fail)++;
    }
}

fn void run_jit_policy_variadic_rest_list_alloc_failure_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: variadic rest list allocation failure skipped when jit disabled");
        (*pass)++;
        return;
    }

    char[] source = "((lambda (.. xs) xs) 1)";
    int saved_force = g_value_force_make_list1_alloc_fail_nth;
    g_value_force_make_list1_alloc_fail_nth = 1;
    JitTopLevelEval jit_eval = eval_jit_top_level(interp, source);
    g_value_force_make_list1_alloc_fail_nth = saved_force;

    Value* result = jit_eval.value;
    bool ok = jit_eval.ok &&
              result != null &&
              result.tag == ERROR &&
              jit_policy_str_contains(result.str_chars[:result.str_len], "out of memory while binding variadic rest arguments");

    if (ok) {
        emit_pass_literal("[PASS] jit policy: variadic rest list allocation failure returns error");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: variadic rest list allocation failure returns error");
        print_jit_failure_debug(interp, "jit variadic rest list allocation failure", jit_eval.stage, result);
        (*fail)++;
    }
}
