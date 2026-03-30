module lisp;

import std::io;
import std::collections::list;
import main;

fn EvalResult parser_error_to_eval_result(Parser* p) {
    EvalResult r;
    r.value = null;
    r.error.has_error = true;
    r.error.line = p.error_line;
    r.error.column = p.error_col;
    usz len = p.error_msg_len;
    if (len > 255) len = 255;
    for (usz i = 0; i < len; i++) {
        r.error.message[i] = p.error_msg[i];
    }
    r.error.message[len] = 0;
    return r;
}

fn bool run_jit_enabled(Interp* interp) @inline {
    if (!interp.flags.jit_enabled) return false;
    // Generated JIT code is not ASAN-instrumented; force interpreter path under ASAN runtime.
    return !main::stack_runtime_asan_enabled();
}

fn void run_clear_stale_raise_state(Interp* interp) @inline {
    // Pending raise is meaningful only while unwinding inside active handlers.
    // At top-level run boundaries (no handlers), stale pending state must not
    // leak into unrelated forms.
    if (interp.handler_count != 0 || !interp.flags.raise_pending) return;
    interp.flags.raise_pending = false;
    interp.raise_msg_len = 0;
    interp.raise_msg[0] = 0;
    interp.raise_payload = null;
}

fn void run_clear_stale_jit_tco_state(Interp* interp) @inline {
    // JIT TCO bounce state is transient to one evaluation and must not leak
    // across top-level run boundaries.
    interp.flags.jit_tco_bounce = false;
    interp.jit_tco_expr = null;
    interp.jit_tco_env = null;
}

fn bool run_stage_trace_enabled() @inline {
    return boundary_c_getenv("OMNI_TRACE_RUN_STAGES") != null;
}

fn bool run_source_matches_filter(char[] source, char* filter) @inline {
    if (filter == null) return true;
    for (usz i = 0; i < source.len; i++) {
        if (filter[i] == 0 || filter[i] != source[i]) return false;
    }
    return filter[source.len] == 0;
}

fn void emit_run_stage_trace(char[] stage, char[] source) {
    if (!run_stage_trace_enabled()) return;
    char* source_filter = boundary_c_getenv("OMNI_TRACE_RUN_SOURCE");
    if (!run_source_matches_filter(source, source_filter)) return;
    io::printfn("OMNI_RUN_TRACE stage=%s source=%s", (ZString)stage, (ZString)source);
}

fn bool run_execute_expr(Expr* expr, Interp* interp, Value** result_value, EvalResult* result_error) {
    if (run_jit_enabled(interp)) {
        RuntimeCompiledExpr f = runtime_compile_expr(expr, interp);
        if (runtime_compiled_expr_is_null(f)) {
            *result_error = eval_error_expr("jit: failed to compile expression during program run", expr);
            return false;
        }
        *result_value = runtime_exec_compiled_expr(f, interp);
        return true;
    }
    *result_value = runtime_eval_expr(expr, interp.global_env, interp);
    return true;
}

fn Value* run_promote_result(Value* jit_result, Interp* interp, main::ScopeRegion* child_scope, main::ScopeRegion* saved_scope, bool* child_spliced) {
    boundary_assert_interp_scope_chain(interp);
    *child_spliced = false;
    ScopedFinalizeResult finalized = boundary_finalize_scoped_result(
        jit_result,
        interp,
        saved_scope,
        child_scope
    );
    *child_spliced = finalized.releasing_scope_spliced;
    return finalized.value;
}

/**
 * Run a complete program.
 */
fn EvalResult run_program(char[] source, Interp* interp) {
    run_clear_stale_raise_state(interp);
    run_clear_stale_jit_tco_state(interp);
    defer run_clear_stale_jit_tco_state(interp);
    dispatch_pending_signal_callbacks(interp);

    Lexer lex;
    lex.init(source);
    defer lex.destroy();
    Parser p;
    p.init(&lex, interp);

    List{Expr*} exprs;
    while (!lex.at_end() && !p.has_error) {
        exprs.push(p.parse_expr());
    }

    if (p.has_error) {
        exprs.free();
        return parser_error_to_eval_result(&p);
    }

    EvalResult result = eval_ok(make_nil(interp));

    foreach (expr : exprs) {
        run_clear_stale_raise_state(interp);
        run_clear_stale_jit_tco_state(interp);
        dispatch_pending_signal_callbacks(interp);

        // GC JIT states between top-level expressions (safe: no JIT code on stack)
        runtime_gc_between_top_level_runs();

        // Expand macros before JIT compilation
        expr = expand_macros_in_expr(expr, interp);
        if (run_jit_enabled(interp)) {
            RuntimeCompiledExpr f = runtime_compile_expr(expr, interp);
            if (!runtime_compiled_expr_is_null(f)) {
                Value* jit_result = runtime_exec_compiled_expr(f, interp);
                if (jit_result != null && jit_result.tag == ERROR) {
                    result = eval_error_expr(jit_result.str_chars[:jit_result.str_len], expr);
                } else {
                    result = eval_ok(jit_result);
                }
                if (result.error.has_error) break;
                continue;
            }
            result = eval_error_expr("jit: failed to compile expression during program run", expr);
            break;
        }
        Value* interpreted = runtime_eval_expr(expr, interp.global_env, interp);
        if (interpreted != null && interpreted.tag == ERROR) {
            result = eval_error_expr(interpreted.str_chars[:interpreted.str_len], expr);
            break;
        }
        result = eval_ok(interpreted);
    }

    exprs.free();
    dispatch_pending_signal_callbacks(interp);
    return result;
}

/**
 * Run a single expression.
 */
fn EvalResult run(char[] source, Interp* interp) {
    emit_run_stage_trace("start", source);
    run_clear_stale_raise_state(interp);
    run_clear_stale_jit_tco_state(interp);
    defer run_clear_stale_jit_tco_state(interp);
    dispatch_pending_signal_callbacks(interp);
    boundary_assert_interp_scope_chain(interp);

    // GC JIT states between top-level evaluations (safe: no JIT code on stack)
    runtime_gc_between_top_level_runs();

    Lexer lex;
    lex.init(source);
    defer lex.destroy();
    Parser p;
    p.init(&lex, interp);

    Expr* expr = p.parse_expr();

    if (p.has_error) {
        emit_run_stage_trace("parse-error", source);
        return parser_error_to_eval_result(&p);
    }

    // JIT path: compile + execute in child region + child scope
    expr = expand_macros_in_expr(expr, interp);

    // Push child scope — temporaries freed after eval
    main::ScopeRegion* saved_scope = null;
    main::ScopeRegion* child_scope = boundary_push_child_scope(interp, &saved_scope);
    if (child_scope == null) {
        emit_run_stage_trace("child-scope-error", source);
        return eval_error("run: failed to create child scope");
    }
    boundary_assert_interp_scope_chain(interp);

    EvalResult result;
    Value* jit_result = null;
    emit_run_stage_trace("before-execute-expr", source);
    if (!run_execute_expr(expr, interp, &jit_result, &result)) {
        emit_run_stage_trace("execute-expr-error", source);
        boundary_pop_child_scope(interp, saved_scope, child_scope);
        return result;
    }
    emit_run_stage_trace("after-execute-expr", source);

    bool child_spliced = false;
    emit_run_stage_trace("before-promote-result", source);
    jit_result = run_promote_result(jit_result, interp, child_scope, saved_scope, &child_spliced);
    emit_run_stage_trace("after-promote-result", source);
    main::ScopeRegion* release_scope = child_spliced ? null : child_scope;

    if (jit_result != null && jit_result.tag == ERROR) {
        result = eval_error_expr(jit_result.str_chars[:jit_result.str_len], expr);
    } else {
        result = eval_ok(jit_result);
    }

    emit_run_stage_trace("before-pop-child-scope", source);
    boundary_pop_child_scope(interp, saved_scope, release_scope);
    emit_run_stage_trace("after-pop-child-scope", source);
    boundary_assert_interp_scope_chain(interp);
    dispatch_pending_signal_callbacks(interp);
    emit_run_stage_trace("end", source);
    return result;
}
