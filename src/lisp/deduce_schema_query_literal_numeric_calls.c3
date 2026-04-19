module lisp;

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
        if (lhs == null || rhs == null || !is_number(lhs) || !is_number(rhs) ||
            is_fixed_complex(lhs) || is_fixed_complex(rhs) || lhs.tag == BIG_COMPLEX || rhs.tag == BIG_COMPLEX) return false;
        bool result;
        if (is_double(lhs) || is_double(rhs) || is_float32(lhs) || is_float32(rhs)) {
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
        if (arg0 == null || !is_number(arg0) || is_fixed_complex(arg0) || arg0.tag == BIG_COMPLEX) return false;
    }
    if (expr.call.arg_count >= 2) {
        if (expr.call.args[1] == null) return false;
        if (!deduce_query_expr_try_extract_literal_value(expr.call.args[1], row_param, capture_env, interp, &arg1)) return false;
        if (arg1 == null || !is_number(arg1) || is_fixed_complex(arg1) || arg1.tag == BIG_COMPLEX) return false;
    }

    if (deduce_name_equals(func_name, "+")) {
        if (expr.call.arg_count == 1) {
            *out_value = arg0;
            return true;
        }
        if (expr.call.arg_count != 2) return false;
        if (is_double(arg0) || is_double(arg1) || is_float32(arg0) || is_float32(arg1)) {
            *out_value = make_double(interp, to_double(arg0) + to_double(arg1));
            return *out_value != null;
        }
        *out_value = make_int(interp, arg0.int_val + arg1.int_val);
        return *out_value != null;
    }
    if (deduce_name_equals(func_name, "-")) {
        if (expr.call.arg_count == 1) {
            if (is_double(arg0) || is_float32(arg0)) {
                *out_value = make_double(interp, -to_double(arg0));
                return *out_value != null;
            }
            *out_value = make_int(interp, -arg0.int_val);
            return *out_value != null;
        }
        if (expr.call.arg_count != 2) return false;
        if (is_double(arg0) || is_double(arg1) || is_float32(arg0) || is_float32(arg1)) {
            *out_value = make_double(interp, to_double(arg0) - to_double(arg1));
            return *out_value != null;
        }
        *out_value = make_int(interp, arg0.int_val - arg1.int_val);
        return *out_value != null;
    }
    if (deduce_name_equals(func_name, "*")) {
        if (expr.call.arg_count != 2) return false;
        if (is_double(arg0) || is_double(arg1) || is_float32(arg0) || is_float32(arg1)) {
            *out_value = make_double(interp, to_double(arg0) * to_double(arg1));
            return *out_value != null;
        }
        *out_value = make_int(interp, arg0.int_val * arg1.int_val);
        return *out_value != null;
    }
    if (deduce_name_equals(func_name, "/")) {
        if (expr.call.arg_count != 2) return false;
        if (is_double(arg0) || is_double(arg1) || is_float32(arg0) || is_float32(arg1)) {
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
        if (is_double(arg0) || is_float32(arg0)) {
            *out_value = make_double(interp, deduce_c_fabs(to_double(arg0)));
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
        if (is_double(arg0) || is_double(arg1) || is_float32(arg0) || is_float32(arg1)) {
            *out_value = make_double(interp, to_double(arg0) < to_double(arg1) ? to_double(arg0) : to_double(arg1));
            return *out_value != null;
        }
        *out_value = make_int(interp, arg0.int_val < arg1.int_val ? arg0.int_val : arg1.int_val);
        return *out_value != null;
    }
    if (deduce_name_equals(func_name, "max")) {
        if (expr.call.arg_count != 2) return false;
        if (is_double(arg0) || is_double(arg1) || is_float32(arg0) || is_float32(arg1)) {
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
    return false;
}
