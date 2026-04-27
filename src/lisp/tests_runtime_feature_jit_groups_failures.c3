module lisp;

import std::io;
import std::core::mem;
import main;

fn bool jit_policy_list_value_is_1_2_3(Value* value) {
    return value != null &&
           value.tag == CONS &&
           value.cons_val.car != null &&
           value.cons_val.car.tag == INT &&
           value.cons_val.car.int_val == 1 &&
           value.cons_val.cdr != null &&
           value.cons_val.cdr.tag == CONS &&
           value.cons_val.cdr.cons_val.car != null &&
           value.cons_val.cdr.cons_val.car.tag == INT &&
           value.cons_val.cdr.cons_val.car.int_val == 2 &&
           value.cons_val.cdr.cons_val.cdr != null &&
           value.cons_val.cdr.cons_val.cdr.tag == CONS &&
           value.cons_val.cdr.cons_val.cdr.cons_val.car != null &&
           value.cons_val.cdr.cons_val.cdr.cons_val.car.tag == INT &&
           value.cons_val.cdr.cons_val.cdr.cons_val.car.int_val == 3 &&
           value.cons_val.cdr.cons_val.cdr.cons_val.cdr != null &&
           value.cons_val.cdr.cons_val.cdr.cons_val.cdr.tag == NIL;
}

fn void run_jit_policy_tail_constructor_escape_opcode_test(Interp* interp, int* pass, int* fail) {
    if (!jit_checks_enabled()) {
        emit_pass_literal("[PASS] jit policy: tail constructor escape opcode skipped when jit disabled");
        (*pass)++;
        return;
    }

    jit_escape_allocation_stats_reset();
    Expr* array_ast = parse_for_jit("(Array 4 5)", interp);
    if (array_ast != null) array_ast = expand_macros_in_expr(array_ast, interp);
    JitLocals classifier_locals = {};
    JitTailConstructorKind array_tail_kind = array_ast != null && array_ast.tag == E_CALL
        ? jit_tail_constructor_kind(array_ast, interp, &classifier_locals, true)
        : JIT_TAIL_CTOR_NONE;
    JitTopLevelEval list_eval = eval_jit_top_level(interp, "(List 1 2 3)");
    JitTopLevelEval array_eval = eval_jit_top_level(interp, "(Array 4 5)");
    JitTopLevelEval string_eval = eval_jit_top_level(interp, "\"jit-tail\"");
    JitEscapeAllocationStats stats = jit_escape_allocation_stats_snapshot();

    bool list_ok = list_eval.ok && jit_policy_list_value_is_1_2_3(list_eval.value);
    bool array_ok = array_eval.ok &&
                    array_eval.value != null &&
                    array_eval.value.tag == ARRAY &&
                    array_eval.value.array_val != null &&
                    array_eval.value.array_val.length == 2 &&
                    array_eval.value.array_val.items[0] != null &&
                    array_eval.value.array_val.items[0].tag == INT &&
                    array_eval.value.array_val.items[0].int_val == 4 &&
                    array_eval.value.array_val.items[1] != null &&
                    array_eval.value.array_val.items[1].tag == INT &&
                    array_eval.value.array_val.items[1].int_val == 5;
    bool string_ok = string_eval.ok &&
                     string_eval.value != null &&
                     string_eval.value.tag == STRING &&
                     string_eval.value.str_len == 8 &&
                     jit_policy_str_contains(string_eval.value.str_chars[:string_eval.value.str_len], "jit-tail");
    bool escape_ok = stats.nil_calls >= 1 &&
                     stats.cons_calls >= 3 &&
                     stats.array_calls >= 1 &&
                     stats.string_calls >= 1;
    bool array_classifier_ok = array_tail_kind == JIT_TAIL_CTOR_ARRAY;

    if (list_ok && array_ok && string_ok && escape_ok && array_classifier_ok) {
        emit_pass_literal("[PASS] jit policy: tail constructor escape opcode uses ESCAPE helpers");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: tail constructor escape opcode uses ESCAPE helpers (list_ok=%s array_ok=%s string_ok=%s escape_ok=%s array_classifier=%s nil=%d cons=%d array=%d string=%d)",
            list_ok ? "yes" : "no",
            array_ok ? "yes" : "no",
            string_ok ? "yes" : "no",
            escape_ok ? "yes" : "no",
            array_classifier_ok ? "yes" : "no",
            (int)stats.nil_calls,
            (int)stats.cons_calls,
            (int)stats.array_calls,
            (int)stats.string_calls
        );
        if (!list_ok) print_jit_failure_debug(interp, "jit tail constructor escape list", list_eval.stage, list_eval.value);
        if (!array_ok) print_jit_failure_debug(interp, "jit tail constructor escape array", array_eval.stage, array_eval.value);
        if (!string_ok) print_jit_failure_debug(interp, "jit tail constructor escape string", string_eval.stage, string_eval.value);
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

fn Value* jit_policy_null_result_primitive(Value*[] args, Env* env, Interp* interp) {
    (void)args;
    (void)env;
    (void)interp;
    return null;
}

fn void run_jit_policy_apply_null_result_fails_closed_test(Interp* interp, int* pass, int* fail) {
    Value* null_prim = make_primitive(interp, "__jit-policy-null-result", &jit_policy_null_result_primitive, 1);
    Value* first_arg = make_int(interp, 1);
    Value* second_arg = make_int(interp, 2);
    Value* one_arg_list = first_arg != null
        ? make_cons(interp, first_arg, make_nil(interp))
        : null;
    Value* two_arg_tail = second_arg != null
        ? make_cons(interp, second_arg, make_nil(interp))
        : null;
    Value* two_arg_list = first_arg != null && two_arg_tail != null
        ? make_cons(interp, first_arg, two_arg_tail)
        : null;

    Value* direct_apply_result = null;
    Value* direct_result = null;
    Value* iterative_result = null;
    bool setup_ok = null_prim != null &&
                    null_prim.tag == PRIMITIVE &&
                    one_arg_list != null &&
                    one_arg_list.tag == CONS &&
                    two_arg_list != null &&
                    two_arg_list.tag == CONS;
    if (setup_ok) {
        direct_apply_result = jit_apply_value(null_prim, first_arg, interp);
        direct_result = jit_apply_multi_args(interp, null_prim, one_arg_list, 1);
        iterative_result = jit_apply_multi_args_iterative(null_prim, two_arg_list, 2, interp);
    }

    bool direct_apply_ok = direct_apply_result != null &&
                           direct_apply_result.tag == ERROR &&
                           jit_policy_str_contains(direct_apply_result.str_chars[:direct_apply_result.str_len], "function application returned null");
    bool direct_ok = direct_result != null &&
                     direct_result.tag == ERROR &&
                     jit_policy_str_contains(direct_result.str_chars[:direct_result.str_len], "function application returned null");
    bool iterative_ok = iterative_result != null &&
                        iterative_result.tag == ERROR &&
                        jit_policy_str_contains(iterative_result.str_chars[:iterative_result.str_len], "function application returned null");
    if (setup_ok && direct_apply_ok && direct_ok && iterative_ok) {
        emit_pass_literal("[PASS] jit policy: null apply result fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: null apply result fails closed (setup_ok=%s apply_tag=%d direct_tag=%d iterative_tag=%d apply_ok=%s direct_ok=%s iterative_ok=%s)",
            setup_ok ? "true" : "false",
            direct_apply_result != null ? (int)direct_apply_result.tag : -1,
            direct_result != null ? (int)direct_result.tag : -1,
            iterative_result != null ? (int)iterative_result.tag : -1,
            direct_apply_ok ? "true" : "false",
            direct_ok ? "true" : "false",
            iterative_ok ? "true" : "false"
        );
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

    Value* result = setup_ok ? jit_apply_value_primitive(invalid, arg, interp) : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
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

fn void run_jit_policy_continuation_wrapper_alloc_failure_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    bool saved_force = g_value_force_make_continuation_wrapper_alloc_fail;

    Interp* other = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = other != null;
    if (ok) {
        other.init();
        g_value_force_make_continuation_wrapper_alloc_fail = true;
        EvalResult r = run("(checkpoint (capture k k))", other);
        g_value_force_make_continuation_wrapper_alloc_fail = saved_force;

        char[] msg = "";
        if (r.error.has_error) {
            msg = r.error.message[:256];
        } else if (r.value != null && r.value.tag == ERROR) {
            msg = r.value.str_chars[:r.value.str_len];
        }
        ok = (r.error.has_error || (r.value != null && r.value.tag == ERROR)) &&
            jit_policy_str_contains(msg, "continuation: failed to allocate wrapper");

        other.destroy();
        mem::free(other);
    } else {
        g_value_force_make_continuation_wrapper_alloc_fail = saved_force;
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: checkpoint continuation wrapper allocation fails closed");
        (*pass)++;
    } else {
        io::printn("[FAIL] jit policy: checkpoint continuation wrapper allocation fails closed");
        (*fail)++;
    }
}



fn void run_jit_policy_handler_arg_list_alloc_failure_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    Interp* other = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = other != null;
    if (ok) {
        other.init();
        ok = register_primitives(other);
    }

    main::StackCtx* ctx = null;
    HandleEffectState* state = null;
    Value* handler_closure = null;
    Value* signal_result = null;
    Value* raise_result = null;
    Continuation* saved_head = null;
    bool signal_msg_ok = false;
    bool raise_msg_ok = false;
    bool continuation_scrubbed = false;
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
        saved_head = other.continuation_head;
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

    signal_msg_ok = signal_result != null &&
        signal_result.tag == ERROR &&
        signal_result.str_chars != null &&
        jit_policy_str_contains(signal_result.str_chars[:signal_result.str_len], "handle: failed to construct handler args");
    raise_msg_ok = raise_result != null &&
        raise_result.tag == ERROR &&
        raise_result.str_chars != null &&
        jit_policy_str_contains(raise_result.str_chars[:raise_result.str_len], "handle: failed to construct raise handler args");
    Continuation* head_after_failure = other != null ? other.continuation_head : null;
    continuation_scrubbed =
        head_after_failure == saved_head ||
        (head_after_failure != null &&
         head_after_failure.next_owned == saved_head &&
         head_after_failure.ctx == null &&
         head_after_failure.handle_state == null &&
         !head_after_failure.handle_retained);
    ok = ok && signal_msg_ok && raise_msg_ok && continuation_scrubbed;

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
            "[FAIL] jit policy: handler arg list construction fails closed (signal_tag=%d raise_tag=%d signal_msg_ok=%s raise_msg_ok=%s continuation_scrubbed=%s)",
            signal_result != null ? (int)signal_result.tag : -1,
            raise_result != null ? (int)raise_result.tag : -1,
            signal_msg_ok ? "true" : "false",
            raise_msg_ok ? "true" : "false",
            continuation_scrubbed ? "true" : "false"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_handle_clause_bind_failure_releases_continuation_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    Interp* other = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = other != null;
    if (ok) {
        other.init();
        ok = register_primitives(other);
    }

    Continuation* saved_head = null;
    EvalResult r = {};
    bool msg_ok = false;
    bool continuation_scrubbed = false;
    if (ok) {
        saved_head = other.continuation_head;
        g_env_force_bindings_alloc_oom = true;
        r = run("(handle (+ 1 (signal ask 0)) (ask x x))", other);
        g_env_force_bindings_alloc_oom = false;
        char[] msg = r.error.message[:256];
        msg_ok = r.error.has_error &&
            jit_policy_str_contains(msg, "handle: out of memory while binding clause");
        Continuation* head_after_failure = other.continuation_head;
        continuation_scrubbed =
            head_after_failure == saved_head ||
            (head_after_failure != null &&
             head_after_failure.next_owned == saved_head &&
             head_after_failure.ctx == null &&
             head_after_failure.handle_state == null &&
             !head_after_failure.handle_retained);
        ok = msg_ok && continuation_scrubbed;
    } else {
        g_env_force_bindings_alloc_oom = false;
    }

    if (other != null) {
        other.destroy();
        mem::free(other);
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: handle clause bind failure releases continuation");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: handle clause bind failure releases continuation (run_error=%s msg_ok=%s msg=%s continuation_scrubbed=%s)",
            r.error.has_error ? "true" : "false",
            msg_ok ? "true" : "false",
            (ZString)&r.error.message,
            continuation_scrubbed ? "true" : "false"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_handler_copy_promotion_failure_cleans_prior_closure_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    Interp* other = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = other != null;
    if (ok) {
        other.init();
        ok = register_primitives(other);
    }

    main::ScopeRegion* saved_scope = null;
    main::ScopeRegion* child = null;
    HandleEffectState* state = null;
    main::ScopeRegion* closure_env_scope = null;
    int closure_env_destroy_count = 0;
    Value* closure = null;
    Value* opaque = null;
    int rc_before_capture = -1;
    int rc_after_capture = -1;
    bool captured = true;
    if (ok) {
        saved_scope = other.current_scope;
        child = main::scope_create(saved_scope);
        ok = child != null;
    }
    if (ok) {
        other.current_scope = child;
        Env* captured_env = make_env(other, other.global_env);
        closure = captured_env != null
            ? make_env_scope_closure_for_test(other, captured_env, &closure_env_scope)
            : null;
        if (closure_env_scope != null) {
            main::scope_register_dtor(closure_env_scope, (void*)&closure_env_destroy_count, fn void(void* ptr) {
                int* count = (int*)ptr;
                if (count != null) (*count)++;
            });
        }
        int* opaque_payload = memory_lifetime_test_opaque_payload(child);
        if (opaque_payload != null) *opaque_payload = 70;
        opaque = opaque_payload != null
            ? make_primitive_with_user_data_in_scope(other, child, "__handler-copy-opaque", &prim_add, 2, (void*)opaque_payload, null, null)
            : null;
        state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
        ok = closure != null &&
             closure.tag == CLOSURE &&
             closure_env_scope != null &&
             closure_env_scope.refcount == 1 &&
             opaque != null &&
             opaque.tag == PRIMITIVE &&
             state != null;
    }
    if (ok) {
        handle_effect_state_init(state, other);
        HandleEffectState* live_state = state;
        state = null;

        SymbolId[2] tags = { other.symbols.intern("first"), other.symbols.intern("second") };
        Value*[2] closures = { closure, opaque };
        EffectHandler handler = {
            .tags = &tags[0],
            .clauses = null,
            .closures = &closures[0],
            .clause_count = 2,
            .handler_env = null,
            .body_expr = null,
            .body_env = null,
            .strict_mode = false,
            .effect_state = live_state,
        };
        rc_before_capture = (int)closure_env_scope.refcount;
        captured = handle_effect_state_capture_handler_copy(live_state, &handler);
        rc_after_capture = (int)closure_env_scope.refcount;
        handle_effect_state_release(live_state);
    }

    if (state != null) mem::free(state);
    if (other != null && saved_scope != null) other.current_scope = saved_scope;
    if (child != null) main::scope_release(child);
    int destroy_count_after_child_release = closure_env_destroy_count;
    if (other != null) {
        other.destroy();
        mem::free(other);
    }

    bool cleanup_ok = ok &&
                      !captured &&
                      rc_before_capture == 1 &&
                      rc_after_capture == 1 &&
                      destroy_count_after_child_release == 1;
    if (cleanup_ok) {
        emit_pass_literal("[PASS] jit policy: handler copy promotion failure cleans prior closure");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: handler copy promotion failure cleans prior closure (setup_ok=%s captured=%s rc_before=%d rc_after=%d destroy_count=%d)",
            ok ? "true" : "false",
            captured ? "true" : "false",
            rc_before_capture,
            rc_after_capture,
            destroy_count_after_child_release
        );
        (*fail)++;
    }
}

fn void run_jit_policy_signal_suspend_failure_clears_handler_state_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    Interp* other = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = other != null;
    if (ok) other.init();

    main::StackCtx* ctx = null;
    HandleEffectState* state = null;
    Value* result = null;
    bool handled = false;
    Continuation* saved_head = null;
    if (ok) {
        ctx = main::stack_ctx_create(&other.stack_ctx_pool);
        ok = ctx != null;
    }
    if (ok) {
        state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
        ok = state != null;
    }
    if (ok) {
        handle_effect_state_init(state, other);
        SymbolId tag = other.symbols.intern("ask");
        SymbolId[1] tags = { tag };
        EffectHandler handler = {
            .tags = &tags[0],
            .clauses = null,
            .closures = null,
            .clause_count = 1,
            .handler_env = null,
            .body_expr = null,
            .body_env = null,
            .strict_mode = false,
            .effect_state = state,
        };
        saved_head = other.continuation_head;
        StackCtxDeferEntry* saved_defer_heap = ctx.defer_heap;
        usz saved_defer_capacity = ctx.defer_capacity;
        other.handler_stack[other.handler_count++] = handler;
        ctx.defer_heap = (StackCtxDeferEntry*)1;
        ctx.defer_capacity = 0;
        handled = jit_signal_try_resume_handler(other, ctx, tag, make_nil(other), &result);
        ctx.defer_heap = saved_defer_heap;
        ctx.defer_capacity = saved_defer_capacity;
        ok = handled &&
             result != null &&
             result.tag == ERROR &&
             jit_policy_str_contains(result.str_chars[:result.str_len], "failed to register suspend scope guard") &&
             !state.signaled &&
             state.signal_tag == (SymbolId)0 &&
             state.signal_arg == null &&
             other.continuation_head == saved_head;
        other.handler_count--;
    }

    if (state != null) handle_effect_state_release(state);
    if (ctx != null) main::stack_ctx_destroy(ctx, &other.stack_ctx_pool);
    if (other != null) {
        other.destroy();
        mem::free(other);
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: signal suspend failure clears handler state");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] jit policy: signal suspend failure clears handler state (handled=%s result_tag=%d signaled=%s)",
            handled ? "yes" : "no",
            result != null ? (int)result.tag : -1,
            state != null && state.signaled ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_jit_policy_shift_suspend_failure_discards_continuation_test(Interp* interp, int* pass, int* fail) {
    (void)interp;
    Interp* other = (Interp*)mem::malloc(Interp.sizeof);
    bool ok = other != null;
    if (ok) other.init();

    main::StackCtx* ctx = null;
    main::StackCtx* saved_current = main::g_current_stack_ctx;
    Continuation* saved_head = null;
    Value* result = null;
    ResetValueState state = {};
    StackCtxDeferEntry* saved_defer_heap = null;
    usz saved_defer_capacity = 0;
    if (ok) {
        ctx = main::stack_ctx_create(&other.stack_ctx_pool);
        ok = ctx != null;
    }
    if (ok) {
        saved_head = other.continuation_head;
        saved_defer_heap = ctx.defer_heap;
        saved_defer_capacity = ctx.defer_capacity;
        state.body = make_nil(other);
        state.interp = other;
        state.shifted = false;
        state.shift_body = null;
        state.shift_k = null;
        other.current_reset_state = &state;
        main::g_current_stack_ctx = ctx;
        ctx.defer_heap = (StackCtxDeferEntry*)1;
        ctx.defer_capacity = 0;
        result = jit_shift_value(other, make_nil(other));
        Continuation* head = other.continuation_head;
        ok = result != null &&
             result.tag == ERROR &&
             jit_policy_str_contains(result.str_chars[:result.str_len], "failed to register suspend scope guard") &&
             head != null &&
             head != saved_head &&
             head.ctx == null &&
             head.handle_state == null &&
             !head.handle_retained &&
             head.next_owned == saved_head &&
             !state.shifted &&
             state.shift_body == null &&
             state.shift_k == null;
        ctx.defer_heap = saved_defer_heap;
        ctx.defer_capacity = saved_defer_capacity;
        if (!ok) {
            io::printfn(
                "[FAIL] jit policy: shift suspend failure discards continuation (result_tag=%d head_ctx=%s head_retained=%s shifted=%s)",
                result != null ? (int)result.tag : -1,
                head != null && head.ctx == null ? "null" : "live",
                head != null && !head.handle_retained ? "no" : "yes",
                state.shifted ? "yes" : "no"
            );
        }
    }

    other.current_reset_state = null;
    main::g_current_stack_ctx = saved_current;
    if (ctx != null) main::stack_ctx_destroy(ctx, &other.stack_ctx_pool);
    if (other != null) {
        other.destroy();
        mem::free(other);
    }

    if (ok) {
        emit_pass_literal("[PASS] jit policy: shift suspend failure discards continuation");
        (*pass)++;
    } else {
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
