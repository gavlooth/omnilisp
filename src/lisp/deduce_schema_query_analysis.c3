module lisp;

import std::core::mem;

fn bool deduce_query_expr_try_extract_literal_value(
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    Interp* interp,
    Value** out_value
) {
    if (out_value == null || expr == null) return false;
    *out_value = null;

    switch (expr.tag) {
        case E_LIT:
            *out_value = expr.lit.value;
            return *out_value != null;
        case E_QUOTE:
            *out_value = expr.quote.datum;
            return *out_value != null;
        case E_BLOCK: {
            if (expr.block == null || expr.block.expr_count == 0 || expr.block.exprs == null) return false;
            Expr* tail = expr.block.exprs[expr.block.expr_count - 1];
            if (tail == null) return false;
            return deduce_query_expr_try_extract_literal_value(
                tail,
                row_param,
                capture_env,
                interp,
                out_value
            );
        }
        case E_LET: {
            if (interp == null || expr.let_expr.init == null || expr.let_expr.body == null) return false;
            if (expr.let_expr.is_recursive || expr.let_expr.pattern != null) return false;

            Value* init_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.let_expr.init,
                    row_param,
                    capture_env,
                    interp,
                    &init_value
                )) return false;
            if (init_value == null) return false;
            Env* parent_env = capture_env != null ? capture_env : interp.global_env;
            if (parent_env == null) return false;
            Env* scoped_env = make_env(interp, parent_env);
            if (scoped_env == null) return false;
            scoped_env.define(expr.let_expr.name, init_value);

            return deduce_query_expr_try_extract_literal_value(
                expr.let_expr.body,
                row_param,
                scoped_env,
                interp,
                out_value
            );
        }
        case E_IF: {
            if (interp == null || expr.if_expr.test == null || expr.if_expr.then_branch == null || expr.if_expr.else_branch == null) {
                return false;
            }
            Value* test_value = null;
            if (!deduce_query_expr_try_extract_literal_value(expr.if_expr.test, row_param, capture_env, interp, &test_value)) return false;
            Expr* selected_branch = is_falsy(test_value, interp)
                ? expr.if_expr.else_branch
                : expr.if_expr.then_branch;
            return deduce_query_expr_try_extract_literal_value(
                selected_branch,
                row_param,
                capture_env,
                interp,
                out_value
            );
        }
        case E_AND: {
            if (interp == null || expr.and_expr.left == null || expr.and_expr.right == null) return false;
            Value* left_value = null;
            if (!deduce_query_expr_try_extract_literal_value(expr.and_expr.left, row_param, capture_env, interp, &left_value)) {
                return false;
            }
            if (is_falsy(left_value, interp)) {
                *out_value = left_value;
                return *out_value != null;
            }
            return deduce_query_expr_try_extract_literal_value(
                expr.and_expr.right,
                row_param,
                capture_env,
                interp,
                out_value
            );
        }
        case E_OR: {
            if (interp == null || expr.or_expr.left == null || expr.or_expr.right == null) return false;
            Value* left_value = null;
            if (!deduce_query_expr_try_extract_literal_value(expr.or_expr.left, row_param, capture_env, interp, &left_value)) {
                return false;
            }
            if (!is_falsy(left_value, interp)) {
                *out_value = left_value;
                return *out_value != null;
            }
            return deduce_query_expr_try_extract_literal_value(
                expr.or_expr.right,
                row_param,
                capture_env,
                interp,
                out_value
            );
        }
        case E_LAMBDA: {
            Env* lookup_env = capture_env != null ? capture_env : (interp != null ? interp.global_env : null);
            if (lookup_env == null || interp == null) return false;
            Value* closure = runtime_eval_expr(expr, lookup_env, interp);
            if (closure != null && !is_error(closure)) {
                *out_value = closure;
                return true;
            }
            return false;
        }
        case E_VAR: {
            if (expr.var_expr.name == row_param) return false;
            Env* lookup_env = capture_env != null ? capture_env : (interp != null ? interp.global_env : null);
            if (lookup_env == null) return false;
            *out_value = lookup_env.lookup(expr.var_expr.name);
            return *out_value != null;
        }
        case E_CALL: {
            if (interp == null || expr.call == null || expr.call.func == null || expr.call.args == null) return false;
            if (expr.call.func.tag != E_VAR) return false;
            char[] func_name = interp.symbols.get_name(expr.call.func.var_expr.name);

            if (deduce_name_equals(func_name, "=")) {
                if (expr.call.arg_count != 2 || expr.call.args[0] == null || expr.call.args[1] == null) return false;
                Value* lhs = null;
                Value* rhs = null;
                if (!deduce_query_expr_try_extract_literal_value(expr.call.args[0], row_param, capture_env, interp, &lhs)) return false;
                if (!deduce_query_expr_try_extract_literal_value(expr.call.args[1], row_param, capture_env, interp, &rhs)) return false;
                *out_value = values_equal(lhs, rhs) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "<") || deduce_name_equals(func_name, "<=")
                || deduce_name_equals(func_name, ">") || deduce_name_equals(func_name, ">=")) {
                if (expr.call.arg_count != 2 || expr.call.args[0] == null || expr.call.args[1] == null) return false;
                Value* lhs = null;
                Value* rhs = null;
                if (!deduce_query_expr_try_extract_literal_value(expr.call.args[0], row_param, capture_env, interp, &lhs)) return false;
                if (!deduce_query_expr_try_extract_literal_value(expr.call.args[1], row_param, capture_env, interp, &rhs)) return false;
                if (lhs == null || rhs == null || !is_number(lhs) || !is_number(rhs)) return false;
                bool result;
                if (is_double(lhs) || is_double(rhs)) {
                    double left = to_double(lhs);
                    double right = to_double(rhs);
                    result = deduce_name_equals(func_name, "<") ? left < right
                        : deduce_name_equals(func_name, "<=") ? left <= right
                        : deduce_name_equals(func_name, ">") ? left > right
                        : left >= right;
                } else {
                    long left = lhs.int_val;
                    long right = rhs.int_val;
                    result = deduce_name_equals(func_name, "<") ? left < right
                        : deduce_name_equals(func_name, "<=") ? left <= right
                        : deduce_name_equals(func_name, ">") ? left > right
                        : left >= right;
                }
                *out_value = result ? make_symbol(interp, interp.sym_true) : make_nil(interp);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "not")) {
                if (expr.call.arg_count != 1 || expr.call.args[0] == null) return false;
                Value* arg = null;
                if (!deduce_query_expr_try_extract_literal_value(expr.call.args[0], row_param, capture_env, interp, &arg)) return false;
                *out_value = is_falsy(arg, interp) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
                return *out_value != null;
            }

            Value* arg0 = null;
            Value* arg1 = null;
            if (expr.call.arg_count >= 1) {
                if (expr.call.args[0] == null) return false;
                if (!deduce_query_expr_try_extract_literal_value(expr.call.args[0], row_param, capture_env, interp, &arg0)) return false;
                if (arg0 == null || !is_number(arg0)) return false;
            }
            if (expr.call.arg_count >= 2) {
                if (expr.call.args[1] == null) return false;
                if (!deduce_query_expr_try_extract_literal_value(expr.call.args[1], row_param, capture_env, interp, &arg1)) return false;
                if (arg1 == null || !is_number(arg1)) return false;
            }

            if (deduce_name_equals(func_name, "+")) {
                if (expr.call.arg_count == 1) {
                    *out_value = arg0;
                    return true;
                }
                if (expr.call.arg_count != 2) return false;
                if (is_double(arg0) || is_double(arg1)) {
                    *out_value = make_double(interp, to_double(arg0) + to_double(arg1));
                    return *out_value != null;
                }
                *out_value = make_int(interp, arg0.int_val + arg1.int_val);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "-")) {
                if (expr.call.arg_count == 1) {
                    if (is_double(arg0)) {
                        *out_value = make_double(interp, -arg0.double_val);
                        return *out_value != null;
                    }
                    *out_value = make_int(interp, -arg0.int_val);
                    return *out_value != null;
                }
                if (expr.call.arg_count != 2) return false;
                if (is_double(arg0) || is_double(arg1)) {
                    *out_value = make_double(interp, to_double(arg0) - to_double(arg1));
                    return *out_value != null;
                }
                *out_value = make_int(interp, arg0.int_val - arg1.int_val);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "*")) {
                if (expr.call.arg_count != 2) return false;
                if (is_double(arg0) || is_double(arg1)) {
                    *out_value = make_double(interp, to_double(arg0) * to_double(arg1));
                    return *out_value != null;
                }
                *out_value = make_int(interp, arg0.int_val * arg1.int_val);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "/")) {
                if (expr.call.arg_count != 2) return false;
                if (is_double(arg0) || is_double(arg1)) {
                    double rhs = to_double(arg1);
                    if (rhs == 0.0) return false;
                    *out_value = make_double(interp, to_double(arg0) / rhs);
                    return *out_value != null;
                }
                if (arg1.int_val == 0) return false;
                *out_value = make_int(interp, arg0.int_val / arg1.int_val);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "abs")) {
                if (expr.call.arg_count != 1) return false;
                if (is_double(arg0)) {
                    *out_value = make_double(interp, deduce_c_fabs(arg0.double_val));
                    return *out_value != null;
                }
                long n = arg0.int_val;
                *out_value = make_int(interp, n < 0 ? -n : n);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "floor")) {
                if (expr.call.arg_count != 1) return false;
                double value = to_double(arg0);
                long out = 0;
                double floor_v = deduce_c_floor(value);
                if (floor_v != floor_v || floor_v > (double)long.max || floor_v < (double)long.min) {
                    return false;
                }
                out = (long)floor_v;
                *out_value = make_int(interp, out);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "ceiling")) {
                if (expr.call.arg_count != 1) return false;
                double value = to_double(arg0);
                double ceil_v = deduce_c_ceil(value);
                if (ceil_v != ceil_v || ceil_v > (double)long.max || ceil_v < (double)long.min) {
                    return false;
                }
                *out_value = make_int(interp, (long)ceil_v);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "round")) {
                if (expr.call.arg_count != 1) return false;
                double value = to_double(arg0);
                double round_v = deduce_c_round(value);
                if (round_v != round_v || round_v > (double)long.max || round_v < (double)long.min) {
                    return false;
                }
                *out_value = make_int(interp, (long)round_v);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "truncate")) {
                if (expr.call.arg_count != 1) return false;
                double value = to_double(arg0);
                if (value != value || value > (double)long.max || value < (double)long.min) {
                    return false;
                }
                *out_value = make_int(interp, (long)value);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "sqrt")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_sqrt(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "exp")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_exp(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "log")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_log(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "log10")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_log10(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "min")) {
                if (expr.call.arg_count != 2) return false;
                if (is_double(arg0) || is_double(arg1)) {
                    *out_value = make_double(interp, to_double(arg0) < to_double(arg1) ? to_double(arg0) : to_double(arg1));
                    return *out_value != null;
                }
                *out_value = make_int(interp, arg0.int_val < arg1.int_val ? arg0.int_val : arg1.int_val);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "max")) {
                if (expr.call.arg_count != 2) return false;
                if (is_double(arg0) || is_double(arg1)) {
                    *out_value = make_double(interp, to_double(arg0) > to_double(arg1) ? to_double(arg0) : to_double(arg1));
                    return *out_value != null;
                }
                *out_value = make_int(interp, arg0.int_val > arg1.int_val ? arg0.int_val : arg1.int_val);
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "pow")) {
                if (expr.call.arg_count != 2) return false;
                *out_value = make_double(interp, deduce_c_pow(to_double(arg0), to_double(arg1)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "sin")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_sin(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "cos")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_cos(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "tan")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_tan(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "asin")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_asin(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "acos")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_acos(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "atan")) {
                if (expr.call.arg_count != 1) return false;
                *out_value = make_double(interp, deduce_c_atan(to_double(arg0)));
                return *out_value != null;
            }
            if (deduce_name_equals(func_name, "atan2")) {
                if (expr.call.arg_count != 2) return false;
                *out_value = make_double(interp, deduce_c_atan2(to_double(arg0), to_double(arg1)));
                return *out_value != null;
            }
            if (expr.call.func.tag == E_VAR) {
                Env* lookup_env = capture_env != null ? capture_env : (interp != null ? interp.global_env : null);
                if (lookup_env != null) {
                    Value* callee = lookup_env.lookup(expr.call.func.var_expr.name);
                    if (callee != null && callee.tag == CLOSURE && callee.closure_val != null) {
                        Closure* closure = callee.closure_val;
                        if (!closure.has_rest &&
                            closure.param_count == 1 &&
                            closure.params != null &&
                            expr.call.arg_count == 1 &&
                            expr.call.args != null &&
                            expr.call.args[0] != null) {
                            Value* call_arg = null;
                            if (!deduce_query_expr_try_extract_literal_value(
                                    expr.call.args[0],
                                    row_param,
                                    capture_env,
                                    interp,
                                    &call_arg
                                )) return false;
                            if (call_arg == null) return false;

                            Env* parent_env = closure.env != null ? closure.env : interp.global_env;
                            if (parent_env == null) return false;
                            Env* call_env = make_env(interp, parent_env);
                            if (call_env == null) return false;
                            call_env.define(closure.params[0], call_arg);

                            if (deduce_query_expr_try_extract_literal_value(
                                closure.body,
                                row_param,
                                call_env,
                                interp,
                                out_value
                            )) {
                            return true;
                            }

                            // Last-resort fallback for closed one-arg wrapper closures.
                            // Evaluate the wrapper body directly in the wrapper call env
                            // so small pure wrappers such as `id` still collapse even if
                            // the recursive extractor cannot simplify the body shape.
                            Value* call_value = runtime_eval_expr(closure.body, call_env, interp);
                            if (call_value == null) {
                                call_value = runtime_eval_expr((Expr*)expr.call, lookup_env, interp);
                            }
                            if (call_value != null) {
                            *out_value = call_value;
                            return true;
                            }
                        }
                    }
                }
            }
            Env* lookup_env = capture_env != null ? capture_env : (interp != null ? interp.global_env : null);
            if (lookup_env != null) {
                Value* call_value = runtime_eval_expr((Expr*)expr.call, lookup_env, interp);
                if (call_value != null) {
                    *out_value = call_value;
                    return true;
                }
            }
            return false;
        }
        default:
            return false;
    }
}

fn bool deduce_query_expr_try_extract_row_column_ref(
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    Relation* rel,
    Interp* interp,
    usz* out_column_index
) {
    if (out_column_index == null || expr == null || rel == null || interp == null) return false;
    *out_column_index = 0;

    Expr* unwrapped_expr = null;
    SymbolId unwrapped_row_param = 0;
    Env* unwrapped_env = null;
    if (deduce_query_expr_try_unwrap_preserved_row_call_chain(
            expr,
            row_param,
            capture_env,
            interp,
            &unwrapped_expr,
            &unwrapped_row_param,
            &unwrapped_env
        )) {
        return deduce_query_expr_try_extract_row_column_ref(
            unwrapped_expr,
            unwrapped_row_param,
            unwrapped_env,
            rel,
            interp,
            out_column_index
        );
    }

    switch (expr.tag) {
        case E_BLOCK: {
            if (expr.block == null || expr.block.expr_count == 0 || expr.block.exprs == null) return false;
            Expr* tail = expr.block.exprs[expr.block.expr_count - 1];
            if (tail == null) return false;
            return deduce_query_expr_try_extract_row_column_ref(
                tail,
                row_param,
                capture_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_LET: {
            if (expr.let_expr.init == null || expr.let_expr.body == null) return false;
            if (expr.let_expr.is_recursive || expr.let_expr.pattern != null) return false;

            Value* init_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.let_expr.init,
                    row_param,
                    capture_env,
                    interp,
                    &init_value
                )) return false;
            if (init_value == null) return false;

            Env* parent_env = capture_env != null ? capture_env : interp.global_env;
            if (parent_env == null) return false;
            Env* scoped_env = make_env(interp, parent_env);
            if (scoped_env == null) return false;
            scoped_env.define(expr.let_expr.name, init_value);

            return deduce_query_expr_try_extract_row_column_ref(
                expr.let_expr.body,
                row_param,
                scoped_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_IF: {
            if (expr.if_expr.test == null || expr.if_expr.then_branch == null || expr.if_expr.else_branch == null) {
                return false;
            }
            Value* test_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.if_expr.test,
                    row_param,
                    capture_env,
                    interp,
                    &test_value
                )) return false;
            Expr* selected_branch = is_falsy(test_value, interp)
                ? expr.if_expr.else_branch
                : expr.if_expr.then_branch;
            return deduce_query_expr_try_extract_row_column_ref(
                selected_branch,
                row_param,
                capture_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_AND: {
            if (expr.and_expr.left == null || expr.and_expr.right == null) return false;
            Value* left_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.and_expr.left,
                    row_param,
                    capture_env,
                    interp,
                    &left_value
                )) return false;
            if (is_falsy(left_value, interp)) {
                return deduce_query_expr_try_extract_row_column_ref(
                    expr.and_expr.left,
                    row_param,
                    capture_env,
                    rel,
                    interp,
                    out_column_index
                );
            }
            return deduce_query_expr_try_extract_row_column_ref(
                expr.and_expr.right,
                row_param,
                capture_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_OR: {
            if (expr.or_expr.left == null || expr.or_expr.right == null) return false;
            Value* left_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    interp,
                    &left_value
                )) return false;
            if (!is_falsy(left_value, interp)) {
                return deduce_query_expr_try_extract_row_column_ref(
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    rel,
                    interp,
                    out_column_index
                );
            }
            return deduce_query_expr_try_extract_row_column_ref(
                expr.or_expr.right,
                row_param,
                capture_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_CALL:
            break;
        default:
            return false;
    }
    if (expr.call == null || expr.call.arg_count != 2) return false;
    if (expr.call.func == null || expr.call.func.tag != E_VAR) return false;
    char[] func_name = interp.symbols.get_name(expr.call.func.var_expr.name);
    if (!deduce_name_equals(func_name, "ref")) return false;
    if (expr.call.args == null || expr.call.args[0] == null || expr.call.args[1] == null) return false;
    if (expr.call.args[0].tag != E_VAR || expr.call.args[0].var_expr.name != row_param) return false;

    Value* column_value = null;
    if (!deduce_query_expr_try_extract_literal_value(expr.call.args[1], row_param, capture_env, interp, &column_value)) return false;
    if (column_value == null || column_value.tag != SYMBOL) return false;
    return deduce_find_column_position(rel.columns, rel.col_count, column_value.sym_val, out_column_index);
}

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

            if (deduce_query_expr_contains_row_param(arg_expr, current_row_param)) return false;

            Value* literal_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    arg_expr,
                    current_row_param,
                    current_env,
                    interp,
                    &literal_value
                )) return false;
            if (literal_value == null) return false;
            call_env.define(closure.params[i], literal_value);
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
