module lisp;

extern fn int c_getrandom(void* buf, usz buflen, uint flags) @extern("getrandom");

fn bool math_fill_random_bytes(void* buf, usz len) {
    char* bytes = (char*)buf;
    usz off = 0;
    while (off < len) {
        int n = c_getrandom(bytes + off, len - off, 0);
        if (n <= 0) return false;
        off += (usz)n;
    }
    return true;
}

fn Value* prim_random(Value*[] args, Env* env, Interp* interp) {
    ulong buf;
    if (!math_fill_random_bytes(&buf, ulong.sizeof)) {
        return raise_error(interp, "random: failed to read random bytes");
    }
    // Convert to double in [0, 1): mask to 52 bits, divide by 2^52
    ulong mantissa = buf & 0x000F_FFFF_FFFF_FFFF;
    double val = (double)mantissa / 4503599627370496.0;  // 2^52
    return make_double(interp, val);
}

fn Value* prim_random_int(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "random-int", "1 argument", args.len);
    long n;
    if (args[0].tag == INT) { n = args[0].int_val; }
    else if (args[0].tag == DOUBLE) {
        if (!math_try_double_to_int(args[0].double_val, &n)) {
            return raise_error(interp, "random-int: expected finite max value in Integer range");
        }
    }
    else if (args[0].tag == FLOAT32) {
        if (!math_try_double_to_int((double)args[0].float32_val, &n)) {
            return raise_error(interp, "random-int: expected finite max value in Integer range");
        }
    }
    else { return primitive_arg_type_raise(interp, "random-int", 0, "number", args[0]); }
    if (n <= 0) return make_int(interp, 0);

    ulong buf;
    if (!math_fill_random_bytes(&buf, ulong.sizeof)) {
        return raise_error(interp, "random-int: failed to read random bytes");
    }
    long result = (long)(buf % (ulong)n);
    return make_int(interp, result);
}
