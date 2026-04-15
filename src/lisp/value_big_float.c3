module lisp;

import std::io;
import main;

const int BIG_FLOAT_OP_ADD = 1;
const int BIG_FLOAT_OP_SUB = 2;
const int BIG_FLOAT_OP_MUL = 3;
const int BIG_FLOAT_OP_DIV = 4;

const int BIG_FLOAT_MATH_SIN = 1;
const int BIG_FLOAT_MATH_COS = 2;
const int BIG_FLOAT_MATH_TAN = 3;
const int BIG_FLOAT_MATH_ASIN = 4;
const int BIG_FLOAT_MATH_ACOS = 5;
const int BIG_FLOAT_MATH_ATAN = 6;
const int BIG_FLOAT_MATH_EXP = 7;
const int BIG_FLOAT_MATH_LOG = 8;
const int BIG_FLOAT_MATH_LOG10 = 9;
const int BIG_FLOAT_MATH_SQRT = 10;
const int BIG_FLOAT_MATH_FLOOR = 11;
const int BIG_FLOAT_MATH_CEILING = 12;
const int BIG_FLOAT_MATH_LGAMMA = 13;
const int BIG_FLOAT_MATH_ERF = 14;
const int BIG_FLOAT_MATH_ERFC = 15;
const int BIG_FLOAT_MATH_NORMAL_CDF = 16;
const int BIG_FLOAT_MATH_NORMAL_QUANTILE = 17;

const int BIG_FLOAT_MATH_POW = 1;
const int BIG_FLOAT_MATH_ATAN2 = 2;

const int BIG_FLOAT_ROUND_FLOOR = 1;
const int BIG_FLOAT_ROUND_CEILING = 2;
const int BIG_FLOAT_ROUND_ROUND = 3;
const int BIG_FLOAT_ROUND_TRUNCATE = 4;

fn Value* make_big_float_from_owned_handle(Interp* interp, void* handle, char[] message) {
    if (handle == null) return raise_error(interp, message);
    Value* v = interp.alloc_value();
    if (v == null) {
        omni_big_float_free(handle);
        return raise_error(interp, "BigFloat: failed to allocate value wrapper");
    }
    v.tag = BIG_FLOAT;
    v.big_float_val = handle;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_value);
    return v;
}

fn Value* make_big_float_from_i64(Interp* interp, long value) {
    return make_big_float_from_owned_handle(
        interp,
        omni_big_float_from_i64(value),
        "BigFloat: failed to allocate float payload");
}

fn Value* make_big_float_from_double(Interp* interp, double value) {
    return make_big_float_from_owned_handle(
        interp,
        omni_big_float_from_double(value),
        "BigFloat: expected finite double");
}

fn Value* make_big_float_from_string(Interp* interp, char[] value) {
    void* handle = omni_big_float_from_chars(value.ptr, value.len);
    if (handle == null) {
        return raise_error_with_payload_names(
            interp,
            "type",
            "type/arg-mismatch",
            "BigFloat: expected finite decimal float string",
            null);
    }
    return make_big_float_from_owned_handle(interp, handle, "BigFloat: failed to allocate float payload");
}

fn Value* copy_big_float_to_parent(Value* v, Interp* interp, BoundaryCopyFault* fault_out = null) {
    if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NONE;

    void* cloned = null;
    if (v.big_float_val != null) {
        cloned = omni_big_float_clone(v.big_float_val);
        if (cloned == null) {
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            return fault_out != null ? null : raise_error(interp, "boundary: failed to clone BigFloat payload");
        }
    }

    Value* result = interp.alloc_value();
    if (result == null) {
        if (cloned != null) omni_big_float_free(cloned);
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate BigFloat copy value");
    }
    result.tag = BIG_FLOAT;
    result.big_float_val = cloned;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* promote_escape_big_float(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;

    void* cloned = null;
    if (v.big_float_val != null) {
        cloned = omni_big_float_clone(v.big_float_val);
        if (cloned == null) return raise_error(interp, "boundary: failed to clone BigFloat escape payload");
    }

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        if (cloned != null) omni_big_float_free(cloned);
        return raise_error(interp, "boundary: failed to allocate BigFloat escape copy value");
    }
    result.tag = BIG_FLOAT;
    result.big_float_val = cloned;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* big_float_to_string_value(Value* v, Interp* interp, char[] message) {
    if (v == null || v.tag != BIG_FLOAT || v.big_float_val == null) return raise_error(interp, message);
    usz len = 0;
    char* rendered = omni_big_float_to_string(v.big_float_val, &len);
    if (rendered == null) return raise_error(interp, message);
    Value* result = make_string(interp, rendered[:len]);
    omni_big_float_string_free(rendered);
    return result;
}

fn void print_big_float_value(Value* v) {
    if (v == null || v.big_float_val == null) {
        io::print("#<big-float:null>");
        return;
    }
    usz len = 0;
    char* rendered = omni_big_float_to_string(v.big_float_val, &len);
    if (rendered == null) {
        io::print("#<big-float:error>");
        return;
    }
    (void)len;
    io::printf("%s", (ZString)rendered);
    omni_big_float_string_free(rendered);
}

fn void print_big_float_value_buf(Value* v, PrintBuf* pb) {
    if (v == null || v.big_float_val == null) {
        pb.append_str("#<big-float:null>");
        return;
    }
    usz len = 0;
    char* rendered = omni_big_float_to_string(v.big_float_val, &len);
    if (rendered == null) {
        pb.append_str("#<big-float:error>");
        return;
    }
    pb.append_str(rendered[:len]);
    omni_big_float_string_free(rendered);
}

fn bool big_float_try_to_double(Value* v, double* out) @inline {
    return v != null &&
        v.tag == BIG_FLOAT &&
        v.big_float_val != null &&
        omni_big_float_to_double(v.big_float_val, out) != 0;
}

fn Value* big_float_from_value(Interp* interp, Value* v, char[] message) {
    if (v == null) return raise_error(interp, message);
    if (v.tag == BIG_FLOAT) return copy_big_float_to_parent(v, interp);
    if (v.tag == INT) return make_big_float_from_i64(interp, v.int_val);
    if (v.tag == DOUBLE) return make_big_float_from_double(interp, v.double_val);
    if (v.tag == BIG_INTEGER) {
        Value* rendered = big_integer_to_string_value(v, interp, "BigFloat: failed to render BigInteger");
        if (rendered == null || rendered.tag != STRING) return raise_error(interp, message);
        return make_big_float_from_string(interp, rendered.str_chars[:rendered.str_len]);
    }
    return raise_error(interp, message);
}

fn void* big_float_handle_from_value(Value* v) {
    if (v == null) return null;
    switch (v.tag) {
        case BIG_FLOAT:
            return omni_big_float_clone(v.big_float_val);
        case INT:
            return omni_big_float_from_i64(v.int_val);
        case DOUBLE:
            return omni_big_float_from_double(v.double_val);
        case BIG_INTEGER: {
            usz len = 0;
            char* rendered = omni_big_integer_to_string(v.big_integer_val, &len);
            if (rendered == null) return null;
            void* result = omni_big_float_from_chars(rendered, len);
            omni_big_integer_string_free(rendered);
            return result;
        }
        default:
            return null;
    }
}

fn Value* big_float_binary_values(Interp* interp, Value* lhs, Value* rhs, int op, char[] message) {
    void* left = big_float_handle_from_value(lhs);
    if (left == null) return raise_error(interp, message);
    defer omni_big_float_free(left);

    void* right = big_float_handle_from_value(rhs);
    if (right == null) return raise_error(interp, message);
    defer omni_big_float_free(right);

    void* result = omni_big_float_binary(left, right, op);
    return make_big_float_from_owned_handle(interp, result, message);
}

fn Value* big_float_neg_value(Interp* interp, Value* v, char[] message) {
    void* source = big_float_handle_from_value(v);
    if (source == null) return raise_error(interp, message);
    defer omni_big_float_free(source);

    void* result = omni_big_float_neg(source);
    return make_big_float_from_owned_handle(interp, result, message);
}

fn Value* big_float_abs_value(Interp* interp, Value* v, char[] message) {
    void* source = big_float_handle_from_value(v);
    if (source == null) return raise_error(interp, message);
    defer omni_big_float_free(source);

    void* result = omni_big_float_abs(source);
    return make_big_float_from_owned_handle(interp, result, message);
}

fn Value* big_float_unary_math_value(Interp* interp, Value* v, int op, char[] message) {
    void* source = big_float_handle_from_value(v);
    if (source == null) return raise_error(interp, message);
    defer omni_big_float_free(source);

    void* result = omni_big_float_unary_math(source, op);
    return make_big_float_from_owned_handle(interp, result, message);
}

fn Value* big_float_binary_math_values(Interp* interp, Value* lhs, Value* rhs, int op, char[] message) {
    void* left = big_float_handle_from_value(lhs);
    if (left == null) return raise_error(interp, message);
    defer omni_big_float_free(left);

    void* right = big_float_handle_from_value(rhs);
    if (right == null) return raise_error(interp, message);
    defer omni_big_float_free(right);

    void* result = omni_big_float_binary_math(left, right, op);
    return make_big_float_from_owned_handle(interp, result, message);
}

fn Value* integer_or_big_integer_from_decimal_string(Interp* interp, char[] value, char[] message) {
    Value* big = make_big_integer_from_string(interp, value);
    if (big == null || big.tag != BIG_INTEGER) return raise_error(interp, message);

    long narrowed = 0;
    if (big_integer_try_to_i64(big, &narrowed)) return make_int(interp, narrowed);
    return big;
}

fn Value* big_float_round_to_integer_value(Interp* interp, Value* v, int op, char[] message) {
    if (v == null || v.tag != BIG_FLOAT || v.big_float_val == null) return raise_error(interp, message);

    usz len = 0;
    char* rendered = omni_big_float_round_to_integer_string(v.big_float_val, op, &len);
    if (rendered == null) return raise_error(interp, message);
    Value* result = integer_or_big_integer_from_decimal_string(interp, rendered[:len], message);
    omni_big_float_string_free(rendered);
    return result;
}

fn bool big_float_compare_values(Value* lhs, Value* rhs, int* out) {
    if (lhs == null || rhs == null || out == null) return false;
    if (lhs.tag == BIG_FLOAT && rhs.tag == BIG_FLOAT) {
        if (lhs.big_float_val == null || rhs.big_float_val == null) return false;
        *out = omni_big_float_compare(lhs.big_float_val, rhs.big_float_val);
        return true;
    }
    if (lhs.tag == BIG_FLOAT && rhs.tag == INT) {
        if (lhs.big_float_val == null) return false;
        *out = omni_big_float_compare_i64(lhs.big_float_val, rhs.int_val);
        return true;
    }
    if (lhs.tag == INT && rhs.tag == BIG_FLOAT) {
        if (rhs.big_float_val == null) return false;
        *out = -omni_big_float_compare_i64(rhs.big_float_val, lhs.int_val);
        return true;
    }
    if (lhs.tag == BIG_FLOAT && rhs.tag == DOUBLE) {
        if (lhs.big_float_val == null) return false;
        *out = omni_big_float_compare_double(lhs.big_float_val, rhs.double_val);
        return true;
    }
    if (lhs.tag == DOUBLE && rhs.tag == BIG_FLOAT) {
        if (rhs.big_float_val == null) return false;
        *out = -omni_big_float_compare_double(rhs.big_float_val, lhs.double_val);
        return true;
    }

    void* left = big_float_handle_from_value(lhs);
    if (left == null) return false;
    defer omni_big_float_free(left);

    void* right = big_float_handle_from_value(rhs);
    if (right == null) return false;
    defer omni_big_float_free(right);

    *out = omni_big_float_compare(left, right);
    return true;
}

fn bool big_float_values_equal(Value* lhs, Value* rhs) @inline {
    int cmp = 0;
    return big_float_compare_values(lhs, rhs, &cmp) && cmp == 0;
}
