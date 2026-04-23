module lisp;

fn Value* big_integer_raise(Interp* interp, String code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "big-integer", code, (String)message, null);
}


import std::io;
import main;

const int BIG_INTEGER_OP_ADD = 1;
const int BIG_INTEGER_OP_SUB = 2;
const int BIG_INTEGER_OP_MUL = 3;
const int BIG_INTEGER_OP_DIV = 4;
const int BIG_INTEGER_OP_MOD = 5;
const int BIG_INTEGER_OP_GCD = 6;
const int BIG_INTEGER_OP_LCM = 7;
const int BIG_INTEGER_OP_BIT_AND = 8;
const int BIG_INTEGER_OP_BIT_OR = 9;
const int BIG_INTEGER_OP_BIT_XOR = 10;
const long BIG_INTEGER_MAX_SHIFT_BITS = 1048576;

fn Value* make_big_integer_from_owned_handle(Interp* interp, void* handle, char[] message) {
    if (handle == null) return big_integer_raise(interp, "big-integer/invalid-argument", message);
    Value* v = interp.alloc_value();
    if (v == null) {
        omni_big_integer_free(handle);
        return big_integer_raise(interp, "big-integer/out-of-memory", "BigInteger: failed to allocate value wrapper");
    }
    v.tag = BIG_INTEGER;
    v.big_integer_val = handle;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_value);
    return v;
}

fn Value* make_big_integer_from_i64(Interp* interp, long value) {
    return make_big_integer_from_owned_handle(
        interp,
        omni_big_integer_from_i64(value),
        "BigInteger: failed to allocate integer payload");
}

fn Value* make_big_integer_from_string(Interp* interp, char[] value) {
    void* handle = omni_big_integer_from_chars(value.ptr, value.len);
    if (handle == null) {
        return raise_error_with_payload_names(
            interp,
            "type",
            "type/arg-mismatch",
            "BigInteger: expected decimal integer string",
            null);
    }
    return make_big_integer_from_owned_handle(interp, handle, "BigInteger: failed to allocate integer payload");
}

fn Value* copy_big_integer_to_parent(Value* v, Interp* interp, BoundaryCopyFault* fault_out = null) {
    if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NONE;

    void* cloned = null;
    if (v.big_integer_val != null) {
        cloned = omni_big_integer_clone(v.big_integer_val);
        if (cloned == null) {
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            return fault_out != null ? null : boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to clone BigInteger payload");
        }
    }

    Value* result = interp.alloc_value();
    if (result == null) {
        if (cloned != null) omni_big_integer_free(cloned);
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate BigInteger copy value");
    }
    result.tag = BIG_INTEGER;
    result.big_integer_val = cloned;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* promote_escape_big_integer(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;

    void* cloned = null;
    if (v.big_integer_val != null) {
        cloned = omni_big_integer_clone(v.big_integer_val);
        if (cloned == null) return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to clone BigInteger escape payload");
    }

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        if (cloned != null) omni_big_integer_free(cloned);
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate BigInteger escape copy value");
    }
    result.tag = BIG_INTEGER;
    result.big_integer_val = cloned;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* big_integer_to_string_value(Value* v, Interp* interp, char[] message) {
    if (v == null || v.tag != BIG_INTEGER || v.big_integer_val == null) {
        return big_integer_raise(interp, "big-integer/invalid-argument", message);
    }
    usz len = 0;
    char* rendered = omni_big_integer_to_string(v.big_integer_val, &len);
    if (rendered == null) return big_integer_raise(interp, "big-integer/invalid-argument", message);
    Value* result = make_string(interp, rendered[:len]);
    omni_big_integer_string_free(rendered);
    return result;
}

fn void print_big_integer_value(Value* v) {
    if (v == null || v.big_integer_val == null) {
        io::print("#<big-integer:null>");
        return;
    }
    usz len = 0;
    char* rendered = omni_big_integer_to_string(v.big_integer_val, &len);
    if (rendered == null) {
        io::print("#<big-integer:error>");
        return;
    }
    (void)len;
    io::printf("%s", (ZString)rendered);
    omni_big_integer_string_free(rendered);
}

fn void print_big_integer_value_buf(Value* v, PrintBuf* pb) {
    if (v == null || v.big_integer_val == null) {
        pb.append_str("#<big-integer:null>");
        return;
    }
    usz len = 0;
    char* rendered = omni_big_integer_to_string(v.big_integer_val, &len);
    if (rendered == null) {
        pb.append_str("#<big-integer:error>");
        return;
    }
    pb.append_str(rendered[:len]);
    omni_big_integer_string_free(rendered);
}

fn bool big_integer_try_to_i64(Value* v, long* out) @inline {
    return v != null &&
        v.tag == BIG_INTEGER &&
        v.big_integer_val != null &&
        omni_big_integer_to_i64(v.big_integer_val, out) != 0;
}

fn bool big_integer_try_to_double(Value* v, double* out) @inline {
    return v != null &&
        v.tag == BIG_INTEGER &&
        v.big_integer_val != null &&
        omni_big_integer_to_double(v.big_integer_val, out) != 0;
}

fn bool exact_integer_value_is_zero(Value* v) @inline {
    if (v == null) return false;
    if (v.tag == INT) return v.int_val == 0;
    if (v.tag == BIG_INTEGER) {
        return v.big_integer_val != null &&
            omni_big_integer_compare_i64(v.big_integer_val, 0) == 0;
    }
    return false;
}

fn bool is_exact_integer_value(Value* v) @inline {
    return v != null && (v.tag == INT || v.tag == BIG_INTEGER);
}

fn bool exact_integer_value_to_i64(Value* v, long* out) @inline {
    if (v == null || out == null) return false;
    if (v.tag == INT) {
        *out = v.int_val;
        return true;
    }
    if (v.tag == BIG_INTEGER) return big_integer_try_to_i64(v, out);
    return false;
}

fn void* big_integer_binary_handle(Value* lhs, Value* rhs, int op) {
    void* result = null;
    if (lhs.tag == INT && rhs.tag == INT) {
        result = omni_big_integer_binary_i64(lhs.int_val, rhs.int_val, op);
    } else if (lhs.tag == INT && rhs.tag == BIG_INTEGER) {
        result = omni_big_integer_binary_left_i64(lhs.int_val, rhs.big_integer_val, op);
    } else if (lhs.tag == BIG_INTEGER && rhs.tag == INT) {
        result = omni_big_integer_binary_right_i64(lhs.big_integer_val, rhs.int_val, op);
    } else if (lhs.tag == BIG_INTEGER && rhs.tag == BIG_INTEGER) {
        result = omni_big_integer_binary(lhs.big_integer_val, rhs.big_integer_val, op);
    }
    return result;
}

fn Value* make_integer_or_big_integer_from_owned_handle(Interp* interp, void* handle, bool prefer_integer, char[] message) {
    if (handle == null) return big_integer_raise(interp, "big-integer/invalid-argument", message);
    if (prefer_integer) {
        long narrowed = 0;
        if (omni_big_integer_to_i64(handle, &narrowed) != 0) {
            omni_big_integer_free(handle);
            return make_int(interp, narrowed);
        }
    }
    return make_big_integer_from_owned_handle(interp, handle, message);
}

fn Value* big_integer_binary_values(Interp* interp, Value* lhs, Value* rhs, int op, char[] message) {
    void* result = big_integer_binary_handle(lhs, rhs, op);
    return make_big_integer_from_owned_handle(interp, result, message);
}

fn Value* big_integer_binary_values_or_integer(Interp* interp, Value* lhs, Value* rhs, int op, char[] message) {
    void* result = big_integer_binary_handle(lhs, rhs, op);
    return make_integer_or_big_integer_from_owned_handle(interp, result, true, message);
}

fn bool numeric_values_compare(Value* lhs, Value* rhs, int* out) {
    if (lhs == null || rhs == null || out == null) return false;
    if (!is_numeric_value(lhs) || !is_numeric_value(rhs)) return false;
    if (lhs.tag == BIG_COMPLEX || rhs.tag == BIG_COMPLEX || is_fixed_complex(lhs) || is_fixed_complex(rhs)) return false;

    if (lhs.tag == BIG_FLOAT || rhs.tag == BIG_FLOAT) {
        return big_float_compare_values(lhs, rhs, out);
    }

    if (lhs.tag == DOUBLE || rhs.tag == DOUBLE || lhs.tag == FLOAT32 || rhs.tag == FLOAT32) {
        double left = 0.0;
        double right = 0.0;
        if (!try_numeric_to_double(lhs, &left) || !try_numeric_to_double(rhs, &right)) return false;
        if (left < right) {
            *out = -1;
        } else if (left > right) {
            *out = 1;
        } else {
            *out = 0;
        }
        return true;
    }

    if (lhs.tag == BIG_INTEGER && rhs.tag == BIG_INTEGER) {
        if (lhs.big_integer_val == null || rhs.big_integer_val == null) return false;
        *out = omni_big_integer_compare(lhs.big_integer_val, rhs.big_integer_val);
        return true;
    }
    if (lhs.tag == BIG_INTEGER && rhs.tag == INT) {
        if (lhs.big_integer_val == null) return false;
        *out = omni_big_integer_compare_i64(lhs.big_integer_val, rhs.int_val);
        return true;
    }
    if (lhs.tag == INT && rhs.tag == BIG_INTEGER) {
        if (rhs.big_integer_val == null) return false;
        *out = -omni_big_integer_compare_i64(rhs.big_integer_val, lhs.int_val);
        return true;
    }
    if (lhs.tag == INT && rhs.tag == INT) {
        if (lhs.int_val < rhs.int_val) {
            *out = -1;
        } else if (lhs.int_val > rhs.int_val) {
            *out = 1;
        } else {
            *out = 0;
        }
        return true;
    }
    return false;
}

fn Value* big_integer_neg_value(Interp* interp, Value* v, char[] message) {
    void* result = null;
    if (v.tag == INT) {
        result = omni_big_integer_neg_i64(v.int_val);
    } else if (v.tag == BIG_INTEGER) {
        result = omni_big_integer_neg(v.big_integer_val);
    }
    return make_big_integer_from_owned_handle(interp, result, message);
}

fn Value* big_integer_bitwise_not_value(Interp* interp, Value* v, bool prefer_integer, char[] message) {
    void* result = null;
    if (v.tag == INT) {
        result = omni_big_integer_bitwise_not_i64(v.int_val);
    } else if (v.tag == BIG_INTEGER) {
        result = omni_big_integer_bitwise_not(v.big_integer_val);
    }
    return make_integer_or_big_integer_from_owned_handle(interp, result, prefer_integer, message);
}

fn Value* big_integer_shift_value(Interp* interp, Value* v, ulong shift, bool left, bool prefer_integer, char[] message) {
    void* result = null;
    if (v.tag == INT) {
        result = left ?
            omni_big_integer_shift_left_i64(v.int_val, shift) :
            omni_big_integer_shift_right_i64(v.int_val, shift);
    } else if (v.tag == BIG_INTEGER) {
        result = left ?
            omni_big_integer_shift_left(v.big_integer_val, shift) :
            omni_big_integer_shift_right(v.big_integer_val, shift);
    }
    return make_integer_or_big_integer_from_owned_handle(interp, result, prefer_integer, message);
}

fn bool big_integer_values_equal(Value* lhs, Value* rhs) @inline {
    if (lhs == null || rhs == null) return false;
    if (lhs.tag == BIG_INTEGER && rhs.tag == BIG_INTEGER) {
        return lhs.big_integer_val != null &&
            rhs.big_integer_val != null &&
            omni_big_integer_compare(lhs.big_integer_val, rhs.big_integer_val) == 0;
    }
    if (lhs.tag == BIG_INTEGER && rhs.tag == INT) {
        return lhs.big_integer_val != null &&
            omni_big_integer_compare_i64(lhs.big_integer_val, rhs.int_val) == 0;
    }
    if (lhs.tag == INT && rhs.tag == BIG_INTEGER) {
        return rhs.big_integer_val != null &&
            omni_big_integer_compare_i64(rhs.big_integer_val, lhs.int_val) == 0;
    }
    return false;
}
