module lisp;

fn bool complex128_is_zero(Complex128Val z) @inline {
    return z.real == 0.0 && z.imag == 0.0;
}

fn bool complex64_is_zero(Complex64Val z) @inline {
    return z.real == 0.0f && z.imag == 0.0f;
}

fn bool fixed_complex_value_to_complex128(Value* value, Complex128Val* out) {
    if (value == null || out == null) return false;
    switch (value.tag) {
        case COMPLEX128:
            *out = value.complex128_val;
            return true;
        case COMPLEX64:
            out.real = (double)value.complex64_val.real;
            out.imag = (double)value.complex64_val.imag;
            return true;
        case DOUBLE:
            out.real = value.double_val;
            out.imag = 0.0;
            return conversion_double_is_finite(out.real);
        case FLOAT32:
            out.real = (double)value.float32_val;
            out.imag = 0.0;
            return true;
        case INT:
            out.real = (double)value.int_val;
            out.imag = 0.0;
            return true;
        case BIG_INTEGER:
            if (!big_integer_try_to_double(value, &out.real)) return false;
            out.imag = 0.0;
            return conversion_double_is_finite(out.real);
        case BIG_FLOAT:
            if (!big_float_try_to_double(value, &out.real)) return false;
            out.imag = 0.0;
            return conversion_double_is_finite(out.real);
        default:
            return false;
    }
}

fn bool fixed_complex_value_to_complex64(Value* value, Complex64Val* out) {
    if (value == null || out == null) return false;
    if (value.tag == COMPLEX64) {
        *out = value.complex64_val;
        return true;
    }
    Complex128Val wide;
    if (!fixed_complex_value_to_complex128(value, &wide)) return false;
    if (!conversion_try_double_to_float32(wide.real, &out.real)) return false;
    if (!conversion_try_double_to_float32(wide.imag, &out.imag)) return false;
    return true;
}

fn bool fixed_complex_value_to_double_if_real(Value* value, double* out) {
    if (value == null || out == null) return false;
    if (value.tag == COMPLEX128) {
        if (value.complex128_val.imag != 0.0) return false;
        *out = value.complex128_val.real;
        return true;
    }
    if (value.tag == COMPLEX64) {
        if (value.complex64_val.imag != 0.0f) return false;
        *out = (double)value.complex64_val.real;
        return true;
    }
    return try_numeric_to_double(value, out);
}

fn bool fixed_complex_values_equal(Value* left, Value* right) {
    Complex128Val a;
    Complex128Val b;
    if (!fixed_complex_value_to_complex128(left, &a)) return false;
    if (!fixed_complex_value_to_complex128(right, &b)) return false;
    return a.real == b.real && a.imag == b.imag;
}

fn double complex128_abs_value(Complex128Val z) @inline {
    return c_sqrt(z.real * z.real + z.imag * z.imag);
}

fn float complex64_abs_value(Complex64Val z) @inline {
    return (float)c_sqrt((double)z.real * (double)z.real + (double)z.imag * (double)z.imag);
}

fn bool fixed_complex_binary128(Complex128Val a, Complex128Val b, int op, Complex128Val* out) {
    if (out == null) return false;
    switch (op) {
        case BIG_INTEGER_OP_ADD:
            out.real = a.real + b.real;
            out.imag = a.imag + b.imag;
            return conversion_double_is_finite(out.real) && conversion_double_is_finite(out.imag);
        case BIG_INTEGER_OP_SUB:
            out.real = a.real - b.real;
            out.imag = a.imag - b.imag;
            return conversion_double_is_finite(out.real) && conversion_double_is_finite(out.imag);
        case BIG_INTEGER_OP_MUL:
            out.real = a.real * b.real - a.imag * b.imag;
            out.imag = a.real * b.imag + a.imag * b.real;
            return conversion_double_is_finite(out.real) && conversion_double_is_finite(out.imag);
        case BIG_INTEGER_OP_DIV: {
            double denom = b.real * b.real + b.imag * b.imag;
            if (denom == 0.0) return false;
            out.real = (a.real * b.real + a.imag * b.imag) / denom;
            out.imag = (a.imag * b.real - a.real * b.imag) / denom;
            return conversion_double_is_finite(out.real) && conversion_double_is_finite(out.imag);
        }
        default:
            return false;
    }
}

fn bool fixed_complex_binary64(Complex64Val a, Complex64Val b, int op, Complex64Val* out) {
    if (out == null) return false;
    Complex128Val wide_a;
    wide_a.real = (double)a.real;
    wide_a.imag = (double)a.imag;
    Complex128Val wide_b;
    wide_b.real = (double)b.real;
    wide_b.imag = (double)b.imag;
    Complex128Val wide_out;
    if (!fixed_complex_binary128(wide_a, wide_b, op, &wide_out)) return false;
    if (!conversion_try_double_to_float32(wide_out.real, &out.real)) return false;
    if (!conversion_try_double_to_float32(wide_out.imag, &out.imag)) return false;
    return true;
}

fn void fixed_complex_append_f64(double real, double imag, PrintBuf* pb) {
    char[64] rbuf;
    pb.append_str(double_to_string(real, &rbuf));
    if (imag < 0.0) {
        pb.append_char('-');
        imag = -imag;
    } else {
        pb.append_char('+');
    }
    char[64] ibuf;
    pb.append_str(double_to_string(imag, &ibuf));
    pb.append_char('i');
}

fn Value* fixed_complex128_to_string_value(Value* value, Interp* interp) {
    char[160] buf;
    PrintBuf pb;
    pb.buf = &buf[0];
    pb.pos = 0;
    pb.capacity = buf.len;
    fixed_complex_append_f64(value.complex128_val.real, value.complex128_val.imag, &pb);
    buf[pb.pos] = 0;
    return make_string(interp, buf[0:pb.pos]);
}

fn Value* fixed_complex64_to_string_value(Value* value, Interp* interp) {
    char[160] buf;
    PrintBuf pb;
    pb.buf = &buf[0];
    pb.pos = 0;
    pb.capacity = buf.len;
    fixed_complex_append_f64((double)value.complex64_val.real, (double)value.complex64_val.imag, &pb);
    buf[pb.pos] = 0;
    return make_string(interp, buf[0:pb.pos]);
}
