module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 4c: EXPRESSION SERIALIZER (Value + Pattern forms)
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

        case STRING:
            self.serialize_string_value_to_buf(v, buf);

        case SYMBOL:
            char[] sym_name = self.interp.symbols.get_name(v.sym_val);
            self.buf_append(buf, sym_name);

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
    char[] dslice = io::bprintf(&dbuf, "%.15g", n)!!;
    self.buf_append(buf, dslice);
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

fn void Compiler.serialize_pattern_to_buf(Compiler* self, Pattern* pat, List{char}* buf) {
    if (pat == null) {
        buf.push('_');
        return;
    }

    switch (pat.tag) {
        case PAT_WILDCARD:
            buf.push('_');

        case PAT_VAR:
            char[] vname = self.interp.symbols.get_name(pat.var_name);
            self.buf_append(buf, vname);

        case PAT_LIT:
            self.serialize_value_to_buf(pat.lit_value, buf);

        case PAT_QUOTE:
            buf.push('\'');
            self.serialize_value_to_buf(pat.quote_datum, buf);

        case PAT_SEQ:
            buf.push('[');
            for (usz i = 0; i < pat.elem_count; i++) {
                if (i > 0) buf.push(' ');
                self.serialize_pattern_to_buf(pat.elements[i], buf);
            }
            if (pat.rest_pos == REST_MIDDLE) {
                self.buf_append(buf, " .. ");
                char[] rname = self.interp.symbols.get_name(pat.rest_binding);
                self.buf_append(buf, rname);
            }
            buf.push(']');

        case PAT_CONS:
            buf.push('(');
            self.serialize_pattern_to_buf(pat.car_pat, buf);
            self.buf_append(buf, " . ");
            self.serialize_pattern_to_buf(pat.cdr_pat, buf);
            buf.push(')');

        default:
            buf.push('_');
    }
}
