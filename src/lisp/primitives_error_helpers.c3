module lisp;

import std::io;
import main;

fn char[] primitive_render_value_shape(Value* value, Interp* interp, char[] buf) {
    if (buf.len == 0) return "";

    char[96] tmp;
    usz rendered_len = print_value_to_buf(value, &interp.symbols, &tmp, tmp.len);
    usz copy_len = rendered_len;
    if (copy_len >= buf.len) copy_len = buf.len - 1;
    for (usz i = 0; i < copy_len; i++) {
        buf[i] = tmp[i];
    }
    buf[copy_len] = 0;
    return buf[:copy_len];
}

fn Value* primitive_arg_type_raise(Interp* interp, char[] prim_name, usz arg_index, char[] expected, Value* actual) {
    char[160] actual_buf;
    char[] actual_text = primitive_render_value_shape(actual, interp, actual_buf[..]);
    if (actual_text.len == 0) actual_text = "nil";

    char[256] msg_buf;
    char[] msg;
    if (try formatted = io::bprintf(
        &msg_buf,
        "%s: argument %d expected %s, got %s",
        (ZString)prim_name,
        (int)(arg_index + 1),
        (ZString)expected,
        (ZString)actual_text
    )) {
        msg = formatted;
    } else {
        msg = "type/arg-mismatch";
    }
    return raise_error_with_payload_names(interp, "type", "type/arg-mismatch", msg, null);
}

fn Value* primitive_arity_raise(Interp* interp, char[] prim_name, char[] expected, usz got) {
    char[256] msg_buf;
    char[] msg;
    if (try formatted = io::bprintf(
        &msg_buf,
        "%s: arity mismatch: expected %s, got %d",
        (ZString)prim_name,
        (ZString)expected,
        (int)got
    )) {
        msg = formatted;
    } else {
        msg = "type/arity";
    }
    return raise_error_with_payload_names(interp, "type", "type/arity", msg, null);
}
