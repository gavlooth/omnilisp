module lisp;

import std::collections::list;
import main;

/**
 * Run a complete program.
 */
fn EvalResult run_program(char[] source, Interp* interp) {
    run_clear_stale_raise_state(interp);
    run_clear_stale_jit_tco_state(interp);
    defer run_clear_stale_jit_tco_state(interp);
    dispatch_pending_signal_callbacks(interp);

    List{Expr*} exprs;
    EvalResult parse_error;
    if (!run_parse_source(source, interp, &exprs, &parse_error)) {
        exprs.free();
        return parse_error;
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

    List{Expr*} exprs;
    EvalResult parse_error;
    if (!run_parse_source(source, interp, &exprs, &parse_error)) {
        exprs.free();
        emit_run_stage_trace("parse-error", source);
        return parse_error;
    }

    if (exprs.len() == 0) {
        exprs.free();
        emit_run_stage_trace("end", source);
        return eval_ok(make_nil(interp));
    }

    // Push child scope; temporaries freed after eval.
    main::ScopeRegion* saved_scope = null;
    main::ScopeRegion* child_scope = boundary_push_child_scope(interp, &saved_scope);
    if (child_scope == null) {
        exprs.free();
        emit_run_stage_trace("child-scope-error", source);
        return eval_error("run: failed to create child scope");
    }
    boundary_assert_interp_scope_chain(interp);

    EvalResult result = eval_ok(make_nil(interp));
    Value* result_value = result.value;
    Expr* last_expr = null;

    foreach (expr : exprs) {
        last_expr = expr;
        run_clear_stale_raise_state(interp);
        run_clear_stale_jit_tco_state(interp);
        dispatch_pending_signal_callbacks(interp);

        // GC JIT states between top-level expressions (safe: no JIT code on stack)
        runtime_gc_between_top_level_runs();

        expr = expand_macros_in_expr(expr, interp);

        emit_run_stage_trace("before-execute-expr", source);
        if (!run_execute_expr(expr, interp, &result_value, &result)) {
            emit_run_stage_trace("execute-expr-error", source);
            exprs.free();
            boundary_pop_child_scope(interp, saved_scope, child_scope);
            return result;
        }
        emit_run_stage_trace("after-execute-expr", source);

        if (result_value != null && result_value.tag == ERROR) {
            result = eval_error_expr(result_value.str_chars[:result_value.str_len], expr);
            break;
        }

        result = eval_ok(result_value);
    }

    exprs.free();

    if (result.error.has_error) {
        emit_run_stage_trace("before-pop-child-scope", source);
        boundary_pop_child_scope(interp, saved_scope, child_scope);
        emit_run_stage_trace("after-pop-child-scope", source);
        boundary_assert_interp_scope_chain(interp);
        dispatch_pending_signal_callbacks(interp);
        emit_run_stage_trace("end", source);
        return result;
    }

    bool child_spliced = false;
    emit_run_stage_trace("before-promote-result", source);
    result_value = run_promote_result(result_value, interp, child_scope, saved_scope, &child_spliced);
    emit_run_stage_trace("after-promote-result", source);
    main::ScopeRegion* release_scope = child_spliced ? null : child_scope;

    if (result_value != null && result_value.tag == ERROR) {
        result = eval_error_expr(result_value.str_chars[:result_value.str_len], last_expr);
    } else {
        result = eval_ok(result_value);
    }

    emit_run_stage_trace("before-pop-child-scope", source);
    boundary_pop_child_scope(interp, saved_scope, release_scope);
    emit_run_stage_trace("after-pop-child-scope", source);
    boundary_assert_interp_scope_chain(interp);
    dispatch_pending_signal_callbacks(interp);
    emit_run_stage_trace("end", source);
    return result;
}
