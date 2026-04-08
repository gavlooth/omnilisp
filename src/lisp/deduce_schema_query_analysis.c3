module lisp;

import std::core::mem;

fn bool deduce_query_expr_try_unwrap_preserved_row_call_chain(
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    Interp* interp,
    Expr** out_expr,
    SymbolId* out_row_param,
    Env** out_capture_env
) {
    if (expr == null || interp == null || out_expr == null || out_row_param == null || out_capture_env == null) {
        return false;
    }

    Expr* current_expr = expr;
    SymbolId current_row_param = row_param;
    Env* current_env = capture_env != null ? capture_env : interp.global_env;
    bool unwrapped_any = false;

    for (usz depth = 0; depth < 4; depth++) {
        if (current_expr == null || current_expr.tag != E_CALL || current_expr.call == null) break;
        if (current_expr.call.func == null || current_expr.call.func.tag != E_VAR) break;
        if (current_expr.call.args == null || current_expr.call.arg_count == 0) break;
        if (current_env == null) return false;

        Value* callee = current_env.lookup(current_expr.call.func.var_expr.name);
        if (callee == null || callee.tag != CLOSURE || callee.closure_val == null) break;

        Closure* closure = callee.closure_val;
        if (closure.has_rest ||
            closure.param_count != current_expr.call.arg_count ||
            closure.params == null ||
            closure.body == null) break;

        usz row_arg_index = usz.max;
        usz row_containing_arg_index = usz.max;
        Env* parent_env = closure.env != null ? closure.env : interp.global_env;
        if (parent_env == null) return false;
        Env* call_env = make_env(interp, parent_env);
        if (call_env == null) return false;

        for (usz i = 0; i < current_expr.call.arg_count; i++) {
            Expr* arg_expr = current_expr.call.args[i];
            if (arg_expr == null) return false;

            if (arg_expr.tag == E_VAR && arg_expr.var_expr.name == current_row_param) {
                if (row_arg_index != usz.max) return false;
                row_arg_index = i;
                continue;
            }

            if (deduce_query_expr_contains_row_param(arg_expr, current_row_param)) {
                if (row_containing_arg_index != usz.max) return false;
                row_containing_arg_index = i;
                continue;
            }

            Value* literal_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    arg_expr,
                    current_row_param,
                    current_env,
                    interp,
                    &literal_value
                )) return false;
            if (literal_value == null) return false;
            if (!call_env.define(closure.params[i], literal_value)) return false;
        }

        if (row_arg_index == usz.max &&
            row_containing_arg_index != usz.max &&
            closure.body != null &&
            closure.body.tag == E_VAR &&
            closure.params != null &&
            closure.body.var_expr.name == closure.params[row_containing_arg_index]) {
            current_expr = current_expr.call.args[row_containing_arg_index];
            current_env = call_env;
            unwrapped_any = true;
            continue;
        }

        if (closure.body != null &&
            closure.body.tag == E_VAR &&
            closure.params != null) {
            usz preserved_arg_index = usz.max;
            for (usz i = 0; i < current_expr.call.arg_count; i++) {
                Expr* arg_expr = current_expr.call.args[i];
                if (arg_expr == null || !deduce_query_expr_contains_row_param(arg_expr, current_row_param)) continue;
                if (preserved_arg_index != usz.max) {
                    preserved_arg_index = usz.max;
                    break;
                }
                preserved_arg_index = i;
            }
            if (preserved_arg_index != usz.max &&
                closure.body.var_expr.name == closure.params[preserved_arg_index]) {
                current_expr = current_expr.call.args[preserved_arg_index];
                current_env = call_env;
                unwrapped_any = true;
                continue;
            }
        }

        if (row_arg_index == usz.max) break;

        current_expr = closure.body;
        current_row_param = closure.params[row_arg_index];
        current_env = call_env;
        unwrapped_any = true;
    }

    if (!unwrapped_any) return false;
    *out_expr = current_expr;
    *out_row_param = current_row_param;
    *out_capture_env = current_env;
    return true;
}
