module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 4c: EXPRESSION SERIALIZER (value + type-annotation forms)
// =============================================================================
fn void Compiler.serialize_value_to_buf(Compiler* self, Value* v, List{char}* buf) {
    if (v == null || v.tag == NIL) {
        self.buf_append(buf, "nil");
        return;
    }

    switch (v.tag) {
        case INT:
            self.serialize_int_value_to_buf(v.int_val, buf);

        case DOUBLE:
            self.serialize_double_value_to_buf(v.double_val, buf);

        case FLOAT32:
            self.buf_append(buf, "(Float32 ");
            self.serialize_double_value_to_buf((double)v.float32_val, buf);
            self.buf_append(buf, ")");

        case COMPLEX128:
            self.buf_append(buf, "(Complex128 ");
            self.serialize_double_value_to_buf(v.complex128_val.real, buf);
            self.buf_append(buf, " ");
            self.serialize_double_value_to_buf(v.complex128_val.imag, buf);
            self.buf_append(buf, ")");

        case COMPLEX64:
            self.buf_append(buf, "(Complex64 ");
            self.serialize_double_value_to_buf((double)v.complex64_val.real, buf);
            self.buf_append(buf, " ");
            self.serialize_double_value_to_buf((double)v.complex64_val.imag, buf);
            self.buf_append(buf, ")");

        case STRING:
            self.serialize_string_value_to_buf(v, buf);

        case SYMBOL:
            char[] sym_name = self.interp.symbols.get_name(v.sym_val);
            self.buf_append(buf, sym_name);

        case TIME_POINT:
            char[96] time_buf;
            char[] time_text = time_point_to_string(v.time_point_val, time_buf[..]);
            self.buf_append(buf, "\"");
            for (usz i = 0; i < time_text.len; i++) {
                char c = time_text[i];
                if (c == '\"') {
                    self.buf_append(buf, "\\\"");
                } else if (c == '\\') {
                    self.buf_append(buf, "\\\\");
                } else if (c == '\n') {
                    self.buf_append(buf, "\\n");
                } else if (c == '\t') {
                    self.buf_append(buf, "\\t");
                } else {
                    buf.push(c);
                }
            }
            self.buf_append(buf, "\"");

        case CONS:
            self.serialize_cons_value_to_buf(v, buf);

        default:
            self.buf_append(buf, "nil");
    }
}

fn void Compiler.serialize_int_value_to_buf(Compiler* self, long n, List{char}* buf) {
    if (n < 0) {
        buf.push('-');
        n = -n;
    }
    if (n == 0) {
        buf.push('0');
        return;
    }

    char[32] digits;
    usz dpos = 0;
    while (n > 0 && dpos < 31) {
        digits[dpos++] = (char)('0' + (n % 10));
        n /= 10;
    }
    for (isz di = (isz)dpos - 1; di >= 0; di--) {
        buf.push(digits[(usz)di]);
    }
}

fn void Compiler.serialize_double_value_to_buf(Compiler* self, double n, List{char}* buf) {
    char[64] dbuf;
    if (try rendered_double = io::bprintf(&dbuf, "%.15g", n)) {
        self.buf_append(buf, rendered_double);
    } else {
        self.buf_append(buf, "0.0");
    }
}

fn void Compiler.serialize_string_value_to_buf(Compiler* self, Value* v, List{char}* buf) {
    buf.push('"');
    for (usz i = 0; i < v.str_len; i++) {
        char c = v.str_chars[i];
        if (c == '"') {
            buf.push('\\');
            buf.push('"');
        } else if (c == '\\') {
            buf.push('\\');
            buf.push('\\');
        } else if (c == '\n') {
            buf.push('\\');
            buf.push('n');
        } else {
            buf.push(c);
        }
    }
    buf.push('"');
}

fn void Compiler.serialize_cons_value_to_buf(Compiler* self, Value* v, List{char}* buf) {
    buf.push('(');
    self.serialize_value_to_buf(v.cons_val.car, buf);
    Value* rest = v.cons_val.cdr;
    while (rest != null && rest.tag == CONS) {
        buf.push(' ');
        self.serialize_value_to_buf(rest.cons_val.car, buf);
        rest = rest.cons_val.cdr;
    }
    if (rest != null && rest.tag != NIL) {
        self.buf_append(buf, " . ");
        self.serialize_value_to_buf(rest, buf);
    }
    buf.push(')');
}
