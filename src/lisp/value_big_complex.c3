module lisp;

import std::io;
import main;

const int BIG_COMPLEX_OP_ADD = 1;
const int BIG_COMPLEX_OP_SUB = 2;
const int BIG_COMPLEX_OP_MUL = 3;
const int BIG_COMPLEX_OP_DIV = 4;

const int BIG_COMPLEX_MATH_SIN = 1;
const int BIG_COMPLEX_MATH_COS = 2;
const int BIG_COMPLEX_MATH_TAN = 3;
const int BIG_COMPLEX_MATH_ASIN = 4;
const int BIG_COMPLEX_MATH_ACOS = 5;
const int BIG_COMPLEX_MATH_ATAN = 6;
const int BIG_COMPLEX_MATH_EXP = 7;
const int BIG_COMPLEX_MATH_LOG = 8;
const int BIG_COMPLEX_MATH_LOG10 = 9;
const int BIG_COMPLEX_MATH_SQRT = 10;
const int BIG_COMPLEX_MATH_SINH = 11;
const int BIG_COMPLEX_MATH_COSH = 12;
const int BIG_COMPLEX_MATH_TANH = 13;

const int BIG_COMPLEX_MATH_POW = 1;

fn Value* make_big_complex_from_owned_handle(Interp* interp, void* handle, char[] message) {
    if (handle == null) return raise_error(interp, message);
    Value* v = interp.alloc_value();
    if (v == null) {
        omni_big_complex_free(handle);
        return raise_error(interp, "BigComplex: failed to allocate value wrapper");
    }
    v.tag = BIG_COMPLEX;
    v.big_complex_val = handle;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_value);
    return v;
}

fn Value* make_big_complex_from_i64(Interp* interp, long value) {
    return make_big_complex_from_owned_handle(
        interp,
        omni_big_complex_from_i64(value),
        "BigComplex: failed to allocate complex payload");
}

fn Value* make_big_complex_from_double(Interp* interp, double value) {
    return make_big_complex_from_owned_handle(
        interp,
        omni_big_complex_from_double(value),
        "BigComplex: expected finite double");
}

fn Value* make_big_complex_from_strings(Interp* interp, char[] real, char[] imag) {
    void* handle = omni_big_complex_from_chars(real.ptr, real.len, imag.ptr, imag.len);
    if (handle == null) {
        return raise_error_with_payload_names(
            interp,
            "type",
            "type/arg-mismatch",
            "BigComplex: expected finite decimal real and imaginary parts",
            null);
    }
    return make_big_complex_from_owned_handle(interp, handle, "BigComplex: failed to allocate complex payload");
}

fn Value* copy_big_complex_to_parent(Value* v, Interp* interp, BoundaryCopyFault* fault_out = null) {
    if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NONE;

    void* cloned = null;
    if (v.big_complex_val != null) {
        cloned = omni_big_complex_clone(v.big_complex_val);
        if (cloned == null) {
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            return fault_out != null ? null : raise_error(interp, "boundary: failed to clone BigComplex payload");
        }
    }

    Value* result = interp.alloc_value();
    if (result == null) {
        if (cloned != null) omni_big_complex_free(cloned);
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate BigComplex copy value");
    }
    result.tag = BIG_COMPLEX;
    result.big_complex_val = cloned;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* promote_escape_big_complex(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;

    void* cloned = null;
    if (v.big_complex_val != null) {
        cloned = omni_big_complex_clone(v.big_complex_val);
        if (cloned == null) return raise_error(interp, "boundary: failed to clone BigComplex escape payload");
    }

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        if (cloned != null) omni_big_complex_free(cloned);
        return raise_error(interp, "boundary: failed to allocate BigComplex escape copy value");
    }
    result.tag = BIG_COMPLEX;
    result.big_complex_val = cloned;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* big_complex_to_string_value(Value* v, Interp* interp, char[] message) {
    if (v == null || v.tag != BIG_COMPLEX || v.big_complex_val == null) return raise_error(interp, message);
    usz len = 0;
    char* rendered = omni_big_complex_to_string(v.big_complex_val, &len);
    if (rendered == null) return raise_error(interp, message);
    Value* result = make_string(interp, rendered[:len]);
    omni_big_complex_string_free(rendered);
    return result;
}

fn void print_big_complex_value(Value* v) {
    if (v == null || v.big_complex_val == null) {
        io::print("#<big-complex:null>");
        return;
    }
    usz len = 0;
    char* rendered = omni_big_complex_to_string(v.big_complex_val, &len);
    if (rendered == null) {
        io::print("#<big-complex:error>");
        return;
    }
    (void)len;
    io::printf("%s", (ZString)rendered);
    omni_big_complex_string_free(rendered);
}

fn void print_big_complex_value_buf(Value* v, PrintBuf* pb) {
    if (v == null || v.big_complex_val == null) {
        pb.append_str("#<big-complex:null>");
        return;
    }
    usz len = 0;
    char* rendered = omni_big_complex_to_string(v.big_complex_val, &len);
    if (rendered == null) {
        pb.append_str("#<big-complex:error>");
        return;
    }
    pb.append_str(rendered[:len]);
    omni_big_complex_string_free(rendered);
}

fn void* big_complex_handle_from_value(Value* v) {
    if (v == null) return null;
    switch (v.tag) {
        case BIG_COMPLEX:
            return omni_big_complex_clone(v.big_complex_val);
        case INT:
            return omni_big_complex_from_i64(v.int_val);
        case DOUBLE:
            return omni_big_complex_from_double(v.double_val);
        case BIG_INTEGER:
        case BIG_FLOAT: {
            void* real = big_float_handle_from_value(v);
            if (real == null) return null;
            void* result = omni_big_complex_from_big_float_parts(real, null);
            omni_big_float_free(real);
            return result;
        }
        default:
            return null;
    }
}

fn Value* big_complex_from_value(Interp* interp, Value* v, char[] message) {
    if (v == null) return raise_error(interp, message);
    if (v.tag == BIG_COMPLEX) return copy_big_complex_to_parent(v, interp);
    void* handle = big_complex_handle_from_value(v);
    return make_big_complex_from_owned_handle(interp, handle, message);
}

fn Value* big_complex_from_parts(Interp* interp, Value* real_value, Value* imag_value, char[] message) {
    void* real = big_float_handle_from_value(real_value);
    if (real == null) return raise_error(interp, message);
    defer omni_big_float_free(real);

    void* imag = big_float_handle_from_value(imag_value);
    if (imag == null) return raise_error(interp, message);
    defer omni_big_float_free(imag);

    void* result = omni_big_complex_from_big_float_parts(real, imag);
    return make_big_complex_from_owned_handle(interp, result, message);
}

fn Value* big_complex_binary_values(Interp* interp, Value* lhs, Value* rhs, int op, char[] message) {
    void* left = big_complex_handle_from_value(lhs);
    if (left == null) return raise_error(interp, message);
    defer omni_big_complex_free(left);

    void* right = big_complex_handle_from_value(rhs);
    if (right == null) return raise_error(interp, message);
    defer omni_big_complex_free(right);

    void* result = omni_big_complex_binary(left, right, op);
    return make_big_complex_from_owned_handle(interp, result, message);
}

fn Value* big_complex_neg_value(Interp* interp, Value* v, char[] message) {
    void* source = big_complex_handle_from_value(v);
    if (source == null) return raise_error(interp, message);
    defer omni_big_complex_free(source);

    void* result = omni_big_complex_neg(source);
    return make_big_complex_from_owned_handle(interp, result, message);
}

fn Value* big_complex_unary_math_value(Interp* interp, Value* v, int op, char[] message) {
    void* source = big_complex_handle_from_value(v);
    if (source == null) return raise_error(interp, message);
    defer omni_big_complex_free(source);

    void* result = omni_big_complex_unary_math(source, op);
    return make_big_complex_from_owned_handle(interp, result, message);
}

fn Value* big_complex_binary_math_values(Interp* interp, Value* lhs, Value* rhs, int op, char[] message) {
    void* left = big_complex_handle_from_value(lhs);
    if (left == null) return raise_error(interp, message);
    defer omni_big_complex_free(left);

    void* right = big_complex_handle_from_value(rhs);
    if (right == null) return raise_error(interp, message);
    defer omni_big_complex_free(right);

    void* result = omni_big_complex_binary_math(left, right, op);
    return make_big_complex_from_owned_handle(interp, result, message);
}

fn Value* big_complex_abs_value(Interp* interp, Value* v, char[] message) {
    void* source = big_complex_handle_from_value(v);
    if (source == null) return raise_error(interp, message);
    defer omni_big_complex_free(source);

    void* result = omni_big_complex_abs(source);
    return make_big_float_from_owned_handle(interp, result, message);
}

fn bool big_complex_value_is_zero(Value* v) {
    void* source = big_complex_handle_from_value(v);
    if (source == null) return false;
    defer omni_big_complex_free(source);
    return omni_big_complex_is_zero(source) != 0;
}

fn bool big_complex_values_equal(Value* lhs, Value* rhs) {
    void* left = big_complex_handle_from_value(lhs);
    if (left == null) return false;
    defer omni_big_complex_free(left);

    void* right = big_complex_handle_from_value(rhs);
    if (right == null) return false;
    defer omni_big_complex_free(right);

    return omni_big_complex_equal(left, right) != 0;
}
