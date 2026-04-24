module lisp;

fn bool deduce_query_literal_ordered_numeric(Value* value) @inline {
    return value != null && is_numeric_value(value) && !is_fixed_complex(value) && value.tag != BIG_COMPLEX;
}

fn bool deduce_query_literal_store_numeric_result(Value* result, Value** out_value) @inline {
    if (result == null || result.tag == ERROR || out_value == null) return false;
    *out_value = result;
    return true;
}

fn bool deduce_query_expr_try_extract_numeric_call_value(
    Expr* expr,
    char[] func_name,
    SymbolId row_param,
    Env* capture_env,
    Interp* interp,
    Value** out_value
) {
    if (expr == null || interp == null || out_value == null) return false;

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
        if (!deduce_query_literal_ordered_numeric(lhs) || !deduce_query_literal_ordered_numeric(rhs)) return false;
        int cmp = 0;
        if (!numeric_values_compare(lhs, rhs, &cmp)) return false;
        bool result = deduce_name_equals(func_name, "<") ? cmp < 0
            : deduce_name_equals(func_name, "<=") ? cmp <= 0
            : deduce_name_equals(func_name, ">") ? cmp > 0
            : cmp >= 0;
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
        if (!deduce_query_literal_ordered_numeric(arg0)) return false;
    }
    if (expr.call.arg_count >= 2) {
        if (expr.call.args[1] == null) return false;
        if (!deduce_query_expr_try_extract_literal_value(expr.call.args[1], row_param, capture_env, interp, &arg1)) return false;
        if (!deduce_query_literal_ordered_numeric(arg1)) return false;
    }

    if (deduce_name_equals(func_name, "+")) {
        if (expr.call.arg_count == 1) {
            *out_value = arg0;
            return true;
        }
        if (expr.call.arg_count != 2) return false;
        return deduce_query_literal_store_numeric_result(numeric_add2(interp, arg0, arg1), out_value);
    }
    if (deduce_name_equals(func_name, "-")) {
        if (expr.call.arg_count == 1) {
            return deduce_query_literal_store_numeric_result(numeric_neg1(interp, arg0), out_value);
        }
        if (expr.call.arg_count != 2) return false;
        return deduce_query_literal_store_numeric_result(numeric_sub2(interp, arg0, arg1), out_value);
    }
    if (deduce_name_equals(func_name, "*")) {
        if (expr.call.arg_count != 2) return false;
        return deduce_query_literal_store_numeric_result(numeric_mul2(interp, arg0, arg1), out_value);
    }
    if (deduce_name_equals(func_name, "/")) {
        if (expr.call.arg_count != 2) return false;
        Value*[2] args = { arg0, arg1 };
        return deduce_query_literal_store_numeric_result(prim_div(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "abs")) {
        if (expr.call.arg_count != 1) return false;
        Value*[1] args = { arg0 };
        return deduce_query_literal_store_numeric_result(prim_abs(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "floor")) {
        if (expr.call.arg_count != 1) return false;
        Value*[1] args = { arg0 };
        return deduce_query_literal_store_numeric_result(prim_floor(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "ceiling")) {
        if (expr.call.arg_count != 1) return false;
        Value*[1] args = { arg0 };
        return deduce_query_literal_store_numeric_result(prim_ceiling(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "round")) {
        if (expr.call.arg_count != 1) return false;
        Value*[1] args = { arg0 };
        return deduce_query_literal_store_numeric_result(prim_round(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "truncate")) {
        if (expr.call.arg_count != 1) return false;
        Value*[1] args = { arg0 };
        return deduce_query_literal_store_numeric_result(prim_truncate(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "sqrt")) {
        if (expr.call.arg_count != 1) return false;
        Value*[1] args = { arg0 };
        return deduce_query_literal_store_numeric_result(prim_sqrt(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "exp")) {
        if (expr.call.arg_count != 1) return false;
        Value*[1] args = { arg0 };
        return deduce_query_literal_store_numeric_result(prim_exp(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "log")) {
        if (expr.call.arg_count != 1) return false;
        Value*[1] args = { arg0 };
        return deduce_query_literal_store_numeric_result(prim_log(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "log10")) {
        if (expr.call.arg_count != 1) return false;
        Value*[1] args = { arg0 };
        return deduce_query_literal_store_numeric_result(prim_log10(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "min")) {
        if (expr.call.arg_count != 2) return false;
        Value*[2] args = { arg0, arg1 };
        return deduce_query_literal_store_numeric_result(prim_min(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "max")) {
        if (expr.call.arg_count != 2) return false;
        Value*[2] args = { arg0, arg1 };
        return deduce_query_literal_store_numeric_result(prim_max(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "pow")) {
        if (expr.call.arg_count != 2) return false;
        Value*[2] args = { arg0, arg1 };
        return deduce_query_literal_store_numeric_result(prim_pow(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "sin")) {
        if (expr.call.arg_count != 1) return false;
        Value*[1] args = { arg0 };
        return deduce_query_literal_store_numeric_result(prim_sin(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "cos")) {
        if (expr.call.arg_count != 1) return false;
        Value*[1] args = { arg0 };
        return deduce_query_literal_store_numeric_result(prim_cos(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "tan")) {
        if (expr.call.arg_count != 1) return false;
        Value*[1] args = { arg0 };
        return deduce_query_literal_store_numeric_result(prim_tan(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "asin")) {
        if (expr.call.arg_count != 1) return false;
        Value*[1] args = { arg0 };
        return deduce_query_literal_store_numeric_result(prim_asin(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "acos")) {
        if (expr.call.arg_count != 1) return false;
        Value*[1] args = { arg0 };
        return deduce_query_literal_store_numeric_result(prim_acos(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "atan")) {
        if (expr.call.arg_count != 1) return false;
        Value*[1] args = { arg0 };
        return deduce_query_literal_store_numeric_result(prim_atan(args[..], null, interp), out_value);
    }
    if (deduce_name_equals(func_name, "atan2")) {
        if (expr.call.arg_count != 2) return false;
        Value*[2] args = { arg0, arg1 };
        return deduce_query_literal_store_numeric_result(prim_atan2(args[..], null, interp), out_value);
    }
    return false;
}
