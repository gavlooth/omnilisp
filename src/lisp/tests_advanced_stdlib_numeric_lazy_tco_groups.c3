module lisp;

import std::io;
import main;

fn void run_advanced_stdlib_introspection_lazy_tco_phase_tests(Interp* interp, int* pass, int* fail,
    bool do_introspection, bool do_lazy_force, bool do_tco) {
    if (do_introspection) {
        // === INTROSPECTION TESTS (Phase 7) ===
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco setup-macro");
        setup(interp, "(define [macro] my-inc (syntax-match ([x] (template (+ (insert x) 1)))))");
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco macroexpand");
        test_truthy(interp, "macroexpand", "(pair? (macroexpand (quote (my-inc 5))))", pass, fail);
        test_error_contains(interp, "macroexpand surfaces improper form conversion error", "(macroexpand (cons 'my-inc (cons 1 2)))",
            "value->expr: expected expression form to be a proper list", pass, fail);
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco eval-literal");
        test_eq(interp, "eval literal", "(eval 42)", 42, pass, fail);
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco eval-list");
        test_eq(interp, "eval list", "(eval (quote (+ 1 2)))", 3, pass, fail);
        test_eq(interp, "eval lambda preserves implicit block body", "((eval '(lambda (x) (set! x 2) x)) 1)",
            2, pass, fail);
        test_eq(interp, "eval let preserves implicit block body", "(eval '(let (x 1) (set! x 2) x))",
            2, pass, fail);
        test_error_contains(interp, "eval rejects improper call form", "(eval (cons '+ (cons 1 2)))",
            "value->expr: expected expression form to be a proper list", pass, fail);
        test_error_contains(interp, "eval rejects improper and form", "(eval (cons 'and (cons true false)))",
            "value->expr: expected expression form to be a proper list", pass, fail);
        test_error_contains(interp, "eval rejects improper lambda params", "(eval (list 'lambda (cons 'x 'y) 'x))",
            "value->expr: expected lambda parameters to be a proper list", pass, fail);
        test_error_contains(interp, "eval rejects short if form", "(eval '(if true 1))",
            "value->expr: if expects exactly 3 arguments", pass, fail);
        test_error_contains(interp, "eval rejects long if form", "(eval '(if true 1 2 3))",
            "value->expr: if expects exactly 3 arguments", pass, fail);
        test_error_contains(interp, "eval rejects long quote form", "(eval '(quote a b))",
            "value->expr: quote expects exactly 1 argument", pass, fail);
        test_error_contains(interp, "eval rejects non-symbol define name", "(eval '(define 42 \"x\"))",
            "value->expr: define name must be a symbol", pass, fail);
        test_error_contains(interp, "eval rejects non-symbol set name", "(eval '(set! 42 1))",
            "value->expr: set! name must be a symbol", pass, fail);
        test_eq(interp, "eval lowers generic set call", "(block (define eval-set-d (Dictionary 'a 1)) (eval '(set! eval-set-d 'b 2)) (ref eval-set-d 'b))",
            2, pass, fail);
        test_error_contains(interp, "eval rejects long checkpoint form", "(eval '(checkpoint 1 2))",
            "value->expr: checkpoint expects exactly 1 argument", pass, fail);
        test_error_contains(interp, "eval rejects long capture form", "(eval '(capture k 1 2))",
            "value->expr: capture expects continuation name and body", pass, fail);
        test_error_contains(interp, "eval rejects non-symbol signal tag", "(eval '(signal 42 1))",
            "value->expr: signal tag must be a symbol", pass, fail);
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco apply");
        test_eq(interp, "apply +", "(apply + (quote (1 2)))", 3, pass, fail);
        test_error_contains(interp, "apply rejects non-list args", "(apply + 1)",
            "apply: expected argument list", pass, fail);
        test_error_contains(interp, "apply rejects improper args", "(apply + (cons 1 2))",
            "apply: expected a proper argument list", pass, fail);
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco bound-yes");
        test_truthy(interp, "bound? yes", "(bound? '+)", pass, fail);
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco bound-no");
        test_nil(interp, "bound? no", "(bound? 'xyzzy-undefined)", pass, fail);
        test_truthy(interp, "bound? Dictionary canonical", "(bound? 'Dictionary)", pass, fail);
        test_truthy(interp, "bound? Dict shorthand", "(bound? 'Dict)", pass, fail);
        test_nil(interp, "bound? dictionary removed", "(bound? 'dictionary)", pass, fail);
        test_nil(interp, "bound? dict removed", "(bound? 'dict)", pass, fail);
        test_truthy(interp, "bound? String canonical", "(bound? 'String)", pass, fail);
        test_nil(interp, "bound? number->string removed", "(bound? 'number->string)", pass, fail);
        test_truthy(interp, "bound? Array canonical", "(bound? 'Array)", pass, fail);
        test_nil(interp, "bound? array removed", "(bound? 'array)", pass, fail);
        test_truthy(interp, "bound? Coroutine canonical", "(bound? 'Coroutine)", pass, fail);
        test_nil(interp, "bound? coroutine removed", "(bound? 'coroutine)", pass, fail);
        test_truthy(interp, "bound? TimePoint canonical", "(bound? 'TimePoint)", pass, fail);
        test_nil(interp, "bound? time-point removed", "(bound? 'time-point)", pass, fail);
        test_nil(interp, "bound? dict-set! removed", "(bound? 'dict-set!)", pass, fail);
        test_nil(interp, "bound? array-set! removed", "(bound? 'array-set!)", pass, fail);
        test_nil(interp, "bound? iterator removed", "(bound? 'iterator)", pass, fail);
        test_nil(interp, "bound? make-iterator removed", "(bound? 'make-iterator)", pass, fail);
        test_nil(interp, "bound? collect removed", "(bound? 'collect)", pass, fail);
        test_nil(interp, "bound? to-array removed", "(bound? 'to-array)", pass, fail);
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco error");
        test_error(interp, "error raises", "(error \"test error\")", pass, fail);
    }

    if (do_lazy_force) {
        // === LAZY EVALUATION TESTS (Phase 8) ===
    emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco setup-delay");
    setup(interp, "(define p (delay (lambda (xx) 42)))");
    SymbolId p_sym = interp.symbols.intern("p");
    Value* p_val = interp.global_env.lookup(p_sym);
    bool p_is_closure = p_val != null && p_val.tag == CLOSURE && p_val.closure_val != null;
    Env* p_env = p_is_closure ? p_val.closure_val.env : null;
    bool p_env_is_global = p_is_closure && p_val.closure_val.env == interp.global_env;
    bool p_env_in_current = p_is_closure && boundary_ptr_in_scope((void*)p_val.closure_val.env, interp.current_scope);
    bool p_env_scope_in_current = p_is_closure &&
        p_val.closure_val.env_scope != null &&
        p_val.closure_val.env_scope == interp.current_scope;
    bool p_env_parent_self = p_env != null && p_env.parent == p_env;
    bool p_env_parent_global = p_env != null && p_env.parent == interp.global_env;
    bool p_env_in_detached_scope = p_is_closure &&
        p_val.closure_val.env_scope != null &&
        boundary_ptr_in_scope((void*)p_env, p_val.closure_val.env_scope);
    bool p_env_cycle = false;
    bool p_env_hits_global = false;
    int p_env_depth = 0;
    SymbolId thunk_sym = interp.symbols.intern("thunk");
    SymbolId result_sym = interp.symbols.intern("result");
    SymbolId forced_sym = interp.symbols.intern("forced");
    Value* thunk_val = p_env != null ? p_env.lookup(thunk_sym) : null;
    Value* result_slot_val = p_env != null ? p_env.lookup(result_sym) : null;
    Value* forced_slot_val = p_env != null ? p_env.lookup(forced_sym) : null;
    Env*[8] p_seen = {};
    Env* p_walk = p_env;
    while (p_walk != null && p_env_depth < 8) {
        for (int i = 0; i < p_env_depth; i++) {
            if (p_seen[i] == p_walk) {
                p_env_cycle = true;
                p_walk = null;
                break;
            }
        }
        if (p_walk == null) break;
        p_seen[p_env_depth] = p_walk;
        if (p_walk == interp.global_env) {
            p_env_hits_global = true;
            p_env_depth++;
            break;
        }
        p_walk = p_walk.parent;
        p_env_depth++;
    }
    if (advanced_group_trace_enabled()) {
        std::io::printfn(
            "OMNI_ADVANCED_GROUP_TRACE state delay-force p_is_closure=%d p_env_scope=%d p_env_is_global=%d p_env_in_current=%d p_env_scope_in_current=%d p_env_in_detached_scope=%d p_env_parent_self=%d p_env_parent_global=%d p_env_cycle=%d p_env_hits_global=%d p_env_depth=%d thunk_sym=%d result_sym=%d forced_sym=%d thunk_tag=%d thunk_scope_gen=%d thunk_env_scope=%d thunk_env_scope_gen=%d result_tag=%d forced_tag=%d raise_pending=%d handler_count=%d p_env_scope_gen=%d current_gen=%d current_escape_gen=%d env_scope_gen=%d",
            p_is_closure ? 1 : 0,
            p_is_closure && p_val.closure_val.env_scope != null ? 1 : 0,
            p_env_is_global ? 1 : 0,
            p_env_in_current ? 1 : 0,
            p_env_scope_in_current ? 1 : 0,
            p_env_in_detached_scope ? 1 : 0,
            p_env_parent_self ? 1 : 0,
            p_env_parent_global ? 1 : 0,
            p_env_cycle ? 1 : 0,
            p_env_hits_global ? 1 : 0,
            p_env_depth,
            (int)thunk_sym,
            (int)result_sym,
            (int)forced_sym,
            thunk_val != null ? (int)thunk_val.tag : -1,
            thunk_val != null && thunk_val.tag == CLOSURE && thunk_val.closure_val != null ? (int)thunk_val.scope_gen : -1,
            thunk_val != null && thunk_val.tag == CLOSURE && thunk_val.closure_val != null && thunk_val.closure_val.env_scope != null ? 1 : 0,
            thunk_val != null && thunk_val.tag == CLOSURE && thunk_val.closure_val != null && thunk_val.closure_val.env_scope != null ? (int)thunk_val.closure_val.env_scope.generation : -1,
            result_slot_val != null ? (int)result_slot_val.tag : -1,
            forced_slot_val != null ? (int)forced_slot_val.tag : -1,
            interp.flags.raise_pending ? 1 : 0,
            (int)interp.handler_count,
            p_env != null ? (int)p_env.scope_gen : -1,
            interp.current_scope != null ? (int)interp.current_scope.generation : -1,
            interp.current_scope != null ? (int)interp.current_scope.escape_generation : -1,
            p_is_closure && p_val.closure_val.env_scope != null ? (int)p_val.closure_val.env_scope.generation : -1
        );
    }
    trace_env_chain_state("delay-force-before", interp, p_env);
    if (advanced_group_trace_enabled() && p_env != null && p_env.binding_count >= 3) {
        std::io::printfn(
            "OMNI_ADVANCED_GROUP_TRACE state delay-force env-bindings-before count=%d b0_name=%d b0_tag=%d b1_name=%d b1_tag=%d b2_name=%d b2_tag=%d",
            (int)p_env.binding_count,
            (int)p_env.bindings[0].name,
            p_env.bindings[0].value != null ? (int)p_env.bindings[0].value.tag : -1,
            (int)p_env.bindings[1].name,
            p_env.bindings[1].value != null ? (int)p_env.bindings[1].value.tag : -1,
            (int)p_env.bindings[2].name,
            p_env.bindings[2].value != null ? (int)p_env.bindings[2].value.tag : -1
        );
    }
    emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force");
    bool saved_jit_enabled = interp.flags.jit_enabled;
    interp.flags.jit_enabled = false;
    run_clear_stale_raise_state(interp);
    run_clear_stale_jit_tco_state(interp);
    dispatch_pending_signal_callbacks(interp);

    EvalResult force_interp;
    Lexer force_lex;
    force_lex.init("(force p)");
    defer force_lex.destroy();
    Parser force_parser;
    force_parser.init(&force_lex, interp);
    emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-parse");
    Expr* force_expr = force_parser.parse_expr();
    if (force_parser.has_error) {
        force_interp = parser_error_to_eval_result(&force_parser);
    } else {
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-expand");
        force_expr = expand_macros_in_expr(force_expr, interp);
        main::ScopeRegion* force_saved_scope = null;
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-push-child");
        main::ScopeRegion* force_child_scope = boundary_push_child_scope(interp, &force_saved_scope);
        if (force_child_scope == null) {
            force_interp = eval_error("force: failed to create child scope");
        } else {
            bool force_child_spliced = false;
            Value* force_value = null;
            emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-before-execute");
            bool force_eval_ok = false;
            if (force_expr != null && force_expr.tag == E_CALL && force_expr.call.arg_count == 1) {
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-eval-func");
                Value* force_func = runtime_eval_expr(force_expr.call.func, interp.global_env, interp);
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-func");
                if (force_func != null && force_func.tag == ERROR) {
                    force_interp = eval_error_expr(force_func.str_chars[:force_func.str_len], force_expr.call.func);
                } else {
                    emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-eval-arg");
                    Value* force_arg = runtime_eval_expr(force_expr.call.args[0], interp.global_env, interp);
                    emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-arg");
                    if (force_arg != null && force_arg.tag == ERROR) {
                        force_interp = eval_error_expr(force_arg.str_chars[:force_arg.str_len], force_expr.call.args[0]);
                    } else {
                        if (advanced_group_trace_enabled()) {
                            std::io::printfn(
                                "OMNI_ADVANCED_GROUP_TRACE state delay-force arg_same_value=%d arg_is_closure=%d arg_env_same=%d arg_env_scope_same=%d",
                                force_arg == p_val ? 1 : 0,
                                force_arg != null && force_arg.tag == CLOSURE && force_arg.closure_val != null ? 1 : 0,
                                force_arg != null &&
                                    force_arg.tag == CLOSURE &&
                                    force_arg.closure_val != null &&
                                    force_arg.closure_val.env == p_env ? 1 : 0,
                                force_arg != null &&
                                    force_arg.tag == CLOSURE &&
                                    force_arg.closure_val != null &&
                                    force_arg.closure_val.env_scope == p_val.closure_val.env_scope ? 1 : 0
                            );
                            if (p_env != null && p_env.binding_count >= 3) {
                                std::io::printfn(
                                    "OMNI_ADVANCED_GROUP_TRACE state delay-force env-bindings-after-arg count=%d b0_name=%d b0_tag=%d b1_name=%d b1_tag=%d b2_name=%d b2_tag=%d",
                                    (int)p_env.binding_count,
                                    (int)p_env.bindings[0].name,
                                    p_env.bindings[0].value != null ? (int)p_env.bindings[0].value.tag : -1,
                                    (int)p_env.bindings[1].name,
                                    p_env.bindings[1].value != null ? (int)p_env.bindings[1].value.tag : -1,
                                    (int)p_env.bindings[2].name,
                                    p_env.bindings[2].value != null ? (int)p_env.bindings[2].value.tag : -1
                                );
                            }
                        }
                        if (advanced_group_trace_enabled() &&
                            force_arg != null &&
                            force_arg.tag == CLOSURE &&
                            force_arg.closure_val != null &&
                            force_arg.closure_val.env != null) {
                            trace_env_chain_state("delay-force-after-arg", interp, force_arg.closure_val.env);
                            Value* direct_thunk = force_arg.closure_val.env.lookup(thunk_sym);
                            Value* direct_thunk_before = direct_thunk;
                            std::io::printfn(
                                "OMNI_ADVANCED_GROUP_TRACE state delay-force direct_thunk_before_tag=%d direct_thunk_before_scope_gen=%d direct_thunk_before_env_scope=%d direct_thunk_before_env_scope_gen=%d direct_thunk_same_as_initial=%d direct_thunk_in_current=%d direct_thunk_in_root=%d direct_thunk_payload_in_current=%d direct_thunk_payload_in_root=%d",
                                direct_thunk_before != null ? (int)direct_thunk_before.tag : -1,
                                direct_thunk_before != null ? (int)direct_thunk_before.scope_gen : -1,
                                direct_thunk_before != null &&
                                    direct_thunk_before.tag == CLOSURE &&
                                    direct_thunk_before.closure_val != null &&
                                    direct_thunk_before.closure_val.env_scope != null ? 1 : 0,
                                direct_thunk_before != null &&
                                    direct_thunk_before.tag == CLOSURE &&
                                    direct_thunk_before.closure_val != null &&
                                    direct_thunk_before.closure_val.env_scope != null ? (int)direct_thunk_before.closure_val.env_scope.generation : -1,
                                direct_thunk_before == thunk_val ? 1 : 0,
                                direct_thunk_before != null && boundary_ptr_in_scope((void*)direct_thunk_before, interp.current_scope) ? 1 : 0,
                                direct_thunk_before != null && boundary_ptr_in_scope((void*)direct_thunk_before, interp.root_scope) ? 1 : 0,
                                direct_thunk_before != null &&
                                    direct_thunk_before.tag == CLOSURE &&
                                    direct_thunk_before.closure_val != null &&
                                    boundary_ptr_in_scope((void*)direct_thunk_before.closure_val, interp.current_scope) ? 1 : 0,
                                direct_thunk_before != null &&
                                    direct_thunk_before.tag == CLOSURE &&
                                    direct_thunk_before.closure_val != null &&
                                    boundary_ptr_in_scope((void*)direct_thunk_before.closure_val, interp.root_scope) ? 1 : 0
                            );
                            emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-direct-thunk-before-apply");
                            Value* direct_thunk_result = direct_thunk != null
                                ? jit_apply_value(direct_thunk, make_nil(interp), interp)
                                : null;
                            emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-direct-thunk-after-apply");
                            Value* direct_thunk_after = force_arg.closure_val.env.lookup(thunk_sym);
                            std::io::printfn(
                                "OMNI_ADVANCED_GROUP_TRACE state delay-force direct_thunk_tag=%d direct_thunk_after_tag=%d direct_thunk_after_same_ptr=%d direct_thunk_result_tag=%d direct_thunk_result_int=%d",
                                direct_thunk != null ? (int)direct_thunk.tag : -1,
                                direct_thunk_after != null ? (int)direct_thunk_after.tag : -1,
                                direct_thunk_after == direct_thunk_before ? 1 : 0,
                                direct_thunk_result != null ? (int)direct_thunk_result.tag : -1,
                                direct_thunk_result != null && direct_thunk_result.tag == INT ? (int)direct_thunk_result.int_val : -1
                            );
                        }
                        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-apply");
                        force_value = jit_apply_value(force_func, force_arg, interp);
                        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-apply");
                        force_eval_ok = true;
                    }
                }
            } else if (!run_execute_expr(force_expr, interp, &force_value, &force_interp)) {
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-execute-error");
            } else {
                force_eval_ok = true;
            }

            if (!force_eval_ok) {
                boundary_pop_child_scope(interp, force_saved_scope, force_child_scope);
            } else {
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-execute");
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-before-promote");
                force_value = run_promote_result(force_value, interp, force_child_scope, force_saved_scope, &force_child_spliced);
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-promote");
                main::ScopeRegion* force_release_scope = force_child_spliced ? null : force_child_scope;
                if (force_value != null && force_value.tag == ERROR) {
                    force_interp = eval_error_expr(force_value.str_chars[:force_value.str_len], force_expr);
                } else {
                    force_interp = eval_ok(force_value);
                }
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-before-pop");
                boundary_pop_child_scope(interp, force_saved_scope, force_release_scope);
                emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-pop");
                dispatch_pending_signal_callbacks(interp);
            }
        }
    }
    interp.flags.jit_enabled = saved_jit_enabled;
    emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-interp");
    bool force_interp_ok =
        !force_interp.error.has_error &&
        is_int(force_interp.value) &&
        force_interp.value.int_val == 42;

    bool force_jit_ok = !jit_checks_enabled();
    char[] force_jit_stage = "disabled";
    Value* force_jit_value = null;
    if (jit_checks_enabled()) {
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-before-jit");
        JitTopLevelEval force_jit = eval_jit_top_level(interp, "(force p)");
        emit_advanced_group_trace("step", "advanced-stdlib-numeric-introspection-lazy-tco force-after-jit");
        force_jit_stage = force_jit.stage;
        force_jit_value = force_jit.value;
        force_jit_ok = force_jit.ok &&
            force_jit_value != null &&
            force_jit_value.tag == INT &&
            force_jit_value.int_val == 42;
    }

    if (force_interp_ok && force_jit_ok) {
        emit_pass("delay/force");
        (*pass)++;
    } else {
        std::io::printfn("[FAIL] %s (interp=%s, jit=%s)", (ZString)"delay/force",
            force_interp_ok ? "ok" : "FAIL",
            force_jit_ok ? "ok" : "FAIL");
        if (!force_jit_ok && jit_checks_enabled()) {
            print_jit_failure_debug(interp, "delay/force", force_jit_stage, force_jit_value);
        }
        (*fail)++;
    }
    }

    if (do_tco) {
        run_advanced_stdlib_tco_tests(interp, pass, fail);
    }
}
