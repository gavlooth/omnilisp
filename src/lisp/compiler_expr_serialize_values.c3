module lisp;

import std::io;
import std::collections::list;
import std::core::mem;
import main;
// =============================================================================
// SECTION 4c: EXPRESSION SERIALIZER (value + type-annotation forms)
// =============================================================================
fn void Compiler.serialize_value_to_buf(Compiler* self, Value* v, List{char}* buf) {
    self.serialize_value_to_buf_depth(v, buf);
}

fn void Compiler.serialize_value_to_buf_depth(Compiler* self, Value* v, List{char}* buf) {
    if (self.has_error) return;
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
            self.serialize_time_point_value_to_buf(v.time_point_val, buf);

        case CONS:
            self.serialize_cons_value_to_buf_depth(v, buf);

        case BIG_INTEGER:
            self.serialize_big_integer_value_to_buf(v, buf);

        case BIG_FLOAT:
            self.serialize_big_float_value_to_buf(v, buf);

        case BIG_COMPLEX:
            self.serialize_big_complex_value_to_buf(v, buf);

        case ARRAY:
            self.serialize_array_value_to_buf(v, buf);

        case HASHMAP:
            self.serialize_hashmap_value_to_buf(v, buf, false);

        case SET:
            self.serialize_hashmap_value_to_buf(v, buf, true);

        case VOID:
            self.buf_append(buf, "Void");

        case CLOSURE:
        case CONTINUATION:
        case PRIMITIVE:
        case PARTIAL_PRIM:
        case ERROR:
        case FFI_HANDLE:
        case TYPE_INFO:
        case INSTANCE:
        case METHOD_TABLE:
        case MODULE:
        case ITERATOR:
        case COROUTINE:
        case TENSOR:
            self.set_unsupported_serialized_value_error(v.tag);

        default:
            self.set_unsupported_serialized_value_error(v.tag);
    }
}

fn void Compiler.set_unsupported_serialized_value_error(Compiler* self, ValueTag tag) {
    if (self.has_error) return;
    char[] name = boundary_value_tag_name(tag);
    if (try msg = io::bprintf(&self.error_storage, "compiler: cannot serialize %s value as source", (ZString)name)) {
        self.set_compile_error(msg);
        return;
    }
    self.set_compile_error("compiler: cannot serialize value as source");
}

fn void Compiler.serialize_int_value_to_buf(Compiler* self, long n, List{char}* buf) {
    char[32] digits;
    char[] rendered = int_to_string(n, digits[..]);
    self.buf_append(buf, rendered);
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

fn void Compiler.serialize_quoted_string_to_buf(Compiler* self, char[] text, List{char}* buf) {
    buf.push('"');
    foreach (c : text) {
        if (c == '"') {
            buf.push('\\');
            buf.push('"');
        } else if (c == '\\') {
            buf.push('\\');
            buf.push('\\');
        } else if (c == '\n') {
            buf.push('\\');
            buf.push('n');
        } else if (c == '\t') {
            buf.push('\\');
            buf.push('t');
        } else {
            buf.push(c);
        }
    }
    buf.push('"');
}

fn void Compiler.serialize_time_point_value_to_buf(Compiler* self, TimePoint tp, List{char}* buf) {
    self.buf_append(buf, "(TimePoint '");
    switch (tp.kind) {
        case TIME_POINT_DATE:
            self.buf_append(buf, "date ");
            self.serialize_int_value_to_buf((long)tp.year, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.month, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.day, buf);
        case TIME_POINT_TIME:
            self.buf_append(buf, "time ");
            self.serialize_int_value_to_buf((long)tp.hour, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.minute, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.second, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.microsecond, buf);
        case TIME_POINT_DATETIME:
            self.buf_append(buf, "datetime ");
            self.serialize_int_value_to_buf((long)tp.year, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.month, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.day, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.hour, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.minute, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.second, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.microsecond, buf);
        case TIME_POINT_DATETIMETZ:
            self.buf_append(buf, "datetime-tz ");
            self.serialize_int_value_to_buf((long)tp.year, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.month, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.day, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.hour, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.minute, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.second, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.microsecond, buf);
            buf.push(' ');
            self.serialize_int_value_to_buf((long)tp.tz_offset, buf);
        default:
            self.set_compile_error("compiler: cannot serialize invalid TimePoint kind");
            return;
    }
    buf.push(')');
}

fn void Compiler.serialize_big_integer_value_to_buf(Compiler* self, Value* v, List{char}* buf) {
    if (v.big_integer_val == null) {
        self.set_unsupported_serialized_value_error(BIG_INTEGER);
        return;
    }
    usz len = 0;
    char* rendered = omni_big_integer_to_string(v.big_integer_val, &len);
    if (rendered == null) {
        self.set_compile_error("compiler: failed to render BigInteger literal");
        return;
    }
    self.buf_append(buf, "(BigInteger ");
    self.serialize_quoted_string_to_buf(rendered[:len], buf);
    buf.push(')');
    omni_big_integer_string_free(rendered);
}

fn void Compiler.serialize_big_float_value_to_buf(Compiler* self, Value* v, List{char}* buf) {
    if (v.big_float_val == null) {
        self.set_unsupported_serialized_value_error(BIG_FLOAT);
        return;
    }
    usz len = 0;
    char* rendered = omni_big_float_to_string(v.big_float_val, &len);
    if (rendered == null) {
        self.set_compile_error("compiler: failed to render BigFloat literal");
        return;
    }
    self.buf_append(buf, "(BigFloat ");
    self.serialize_quoted_string_to_buf(rendered[:len], buf);
    buf.push(')');
    omni_big_float_string_free(rendered);
}

fn void Compiler.serialize_big_complex_part_to_buf(Compiler* self, void* part, List{char}* buf) {
    if (part == null) {
        self.set_compile_error("compiler: failed to render BigComplex literal");
        return;
    }
    usz len = 0;
    char* rendered = omni_big_float_to_string(part, &len);
    if (rendered == null) {
        self.set_compile_error("compiler: failed to render BigComplex literal");
        omni_big_float_free(part);
        return;
    }
    self.serialize_quoted_string_to_buf(rendered[:len], buf);
    omni_big_float_string_free(rendered);
    omni_big_float_free(part);
}

fn void Compiler.serialize_big_complex_value_to_buf(Compiler* self, Value* v, List{char}* buf) {
    if (v.big_complex_val == null) {
        self.set_unsupported_serialized_value_error(BIG_COMPLEX);
        return;
    }
    void* real_part = omni_big_complex_real_part(v.big_complex_val);
    void* imag_part = omni_big_complex_imag_part(v.big_complex_val);
    self.buf_append(buf, "(BigComplex ");
    self.serialize_big_complex_part_to_buf(real_part, buf);
    if (self.has_error) {
        if (imag_part != null) omni_big_float_free(imag_part);
        return;
    }
    buf.push(' ');
    self.serialize_big_complex_part_to_buf(imag_part, buf);
    if (self.has_error) return;
    buf.push(')');
}

fn void Compiler.serialize_array_value_to_buf(Compiler* self, Value* v, List{char}* buf) {
    if (v.array_val == null) {
        self.set_unsupported_serialized_value_error(ARRAY);
        return;
    }
    buf.push('[');
    for (usz i = 0; i < v.array_val.length; i++) {
        if (i > 0) buf.push(' ');
        self.serialize_value_to_buf_depth(v.array_val.items[i], buf);
        if (self.has_error) return;
    }
    buf.push(']');
}

fn void Compiler.serialize_hashmap_value_to_buf(Compiler* self, Value* v, List{char}* buf, bool is_set_value) {
    if (v.hashmap_val == null) {
        self.set_unsupported_serialized_value_error(v.tag);
        return;
    }
    if (!hashmap_storage_valid(v.hashmap_val)) {
        self.set_compile_error("compiler: dictionary backing entries are missing");
        return;
    }
    uint[] slots = hashmap_sorted_slots(v.hashmap_val, self.interp);
    if (slots.ptr == null && v.hashmap_val.count > 0) {
        self.set_compile_error("compiler: failed to allocate dictionary serialization order");
        return;
    }
    defer if (slots.ptr != null) mem::free(slots.ptr);

    if (is_set_value) {
        self.buf_append(buf, "(Set");
        for (usz i = 0; i < slots.len; i++) {
            uint slot = slots[i];
            buf.push(' ');
            self.serialize_value_to_buf_depth(v.hashmap_val.entries[slot].key, buf);
            if (self.has_error) return;
        }
        buf.push(')');
        return;
    }

    buf.push('{');
    for (usz i = 0; i < slots.len; i++) {
        uint slot = slots[i];
        if (i > 0) buf.push(' ');
        self.serialize_value_to_buf_depth(v.hashmap_val.entries[slot].key, buf);
        if (self.has_error) return;
        buf.push(' ');
        self.serialize_value_to_buf_depth(v.hashmap_val.entries[slot].value, buf);
        if (self.has_error) return;
    }
    buf.push('}');
}

fn void Compiler.serialize_cons_value_to_buf_depth(Compiler* self, Value* v, List{char}* buf) {
    buf.push('(');
    self.serialize_value_to_buf_depth(v.cons_val.car, buf);
    Value* rest = v.cons_val.cdr;
    while (rest != null && rest.tag == CONS) {
        buf.push(' ');
        self.serialize_value_to_buf_depth(rest.cons_val.car, buf);
        if (self.has_error) return;
        rest = rest.cons_val.cdr;
    }
    if (rest != null && rest.tag != NIL) {
        self.buf_append(buf, " . ");
        self.serialize_value_to_buf_depth(rest, buf);
        if (self.has_error) return;
    }
    buf.push(')');
}
