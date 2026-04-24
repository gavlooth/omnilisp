module lisp;

import std::io;

// =============================================================================
// print_value_to_buf — capture print_value output to a char buffer
// =============================================================================

struct PrintBuf {
    char* buf;
    usz pos;
    usz capacity;
}

fn void PrintBuf.append_str(PrintBuf* self, char[] s) {
    if (self == null || self.buf == null || self.capacity == 0) return;
    foreach (c : s) {
        if (self.pos >= self.capacity - 1) break;
        self.buf[self.pos++] = c;
    }
}

fn void PrintBuf.append_char(PrintBuf* self, char c) {
    if (self == null || self.buf == null || self.capacity == 0) return;
    if (self.pos < self.capacity - 1) {
        self.buf[self.pos++] = c;
    }
}

fn void PrintBuf.append_zstr(PrintBuf* self, ZString zs) {
    if (self == null || self.buf == null || self.capacity == 0 || zs == null) return;
    char* p = (char*)zs;
    while (*p != 0 && self.pos < self.capacity - 1) {
        self.buf[self.pos++] = *p;
        p++;
    }
}

fn char value_print_hex_digit(uint nibble) @inline {
    if (nibble < 10) return (char)((uint)'0' + nibble);
    return (char)((uint)'A' + (nibble - 10));
}

fn bool value_print_needs_hex_escape(char c) @inline {
    uint b = ((uint)c) & 0xFF;
    return b < 0x20 || b == 0x7F;
}

fn void PrintBuf.append_hex_escape(PrintBuf* self, char c) {
    uint b = ((uint)c) & 0xFF;
    self.append_char('\\');
    self.append_char('x');
    self.append_char(value_print_hex_digit((b >> 4) & 0xF));
    self.append_char(value_print_hex_digit(b & 0xF));
}

fn void PrintBuf.append_escaped_string_content(PrintBuf* self, char[] bytes) {
    foreach (c : bytes) {
        if (c == '"') {
            self.append_str("\\\"");
        } else if (c == '\\') {
            self.append_str("\\\\");
        } else if (c == '\n') {
            self.append_str("\\n");
        } else if (c == '\r') {
            self.append_str("\\r");
        } else if (c == '\t') {
            self.append_str("\\t");
        } else if (value_print_needs_hex_escape(c)) {
            self.append_hex_escape(c);
        } else {
            self.append_char(c);
        }
    }
}

fn void value_print_emit_hex_escape(char c) {
    uint b = ((uint)c) & 0xFF;
    char[4] escaped = {
        '\\',
        'x',
        value_print_hex_digit((b >> 4) & 0xF),
        value_print_hex_digit(b & 0xF)
    };
    io::print(escaped[..]);
}

fn void value_print_emit_raw_byte(char c) {
    char[1] out = { c };
    io::print(out[..]);
}

fn void value_print_emit_escaped_string_content(char[] bytes) {
    foreach (c : bytes) {
        if (c == '"') {
            io::print("\\\"");
        } else if (c == '\\') {
            io::print("\\\\");
        } else if (c == '\n') {
            io::print("\\n");
        } else if (c == '\r') {
            io::print("\\r");
        } else if (c == '\t') {
            io::print("\\t");
        } else if (value_print_needs_hex_escape(c)) {
            value_print_emit_hex_escape(c);
        } else {
            value_print_emit_raw_byte(c);
        }
    }
}

fn void print_cons_value_buf(Value* v, SymbolTable* syms, PrintBuf* pb) {
    pb.append_char('(');
    print_value_buf(v.cons_val.car, syms, pb);
    Value* rest = v.cons_val.cdr;
    while (is_cons(rest)) {
        pb.append_char(' ');
        print_value_buf(rest.cons_val.car, syms, pb);
        rest = rest.cons_val.cdr;
    }
    if (!is_nil(rest)) {
        pb.append_str(" . ");
        print_value_buf(rest, syms, pb);
    }
    pb.append_char(')');
}

fn void print_closure_value_buf(Value* v, SymbolTable* syms, PrintBuf* pb) {
    pb.append_str("#<\xCE\xBB");
    if ((uint)v.closure_val.name != 0) {
        pb.append_char(' ');
        pb.append_str((String)syms.get_name(v.closure_val.name));
    }
    pb.append_str(" (");
    for (usz i = 0; i < v.closure_val.param_count; i++) {
        if (i > 0) pb.append_char(' ');
        pb.append_str((String)syms.get_name(v.closure_val.params[i]));
    }
    if (v.closure_val.has_rest) {
        if (v.closure_val.param_count > 0) pb.append_char(' ');
        pb.append_str(".. ");
        pb.append_str((String)syms.get_name(v.closure_val.rest_param));
    }
    pb.append_str(")>");
}

fn void print_hashmap_value_buf(Value* v, SymbolTable* syms, PrintBuf* pb) {
    pb.append_char('{');
    if (v.hashmap_val == null) {
        pb.append_char('}');
        return;
    }
    uint hprinted = 0;
    foreach (entry : v.hashmap_val.entries[0:v.hashmap_val.capacity]) {
        if (entry.key != null) {
            if (hprinted > 0) pb.append_char(' ');
            print_value_buf(entry.key, syms, pb);
            pb.append_char(' ');
            print_value_buf(entry.value, syms, pb);
            hprinted++;
        }
    }
    pb.append_char('}');
}

fn void print_set_value_buf(Value* v, SymbolTable* syms, PrintBuf* pb) {
    pb.append_str("(Set");
    if (v.hashmap_val == null) {
        pb.append_char(')');
        return;
    }
    foreach (entry : v.hashmap_val.entries[0:v.hashmap_val.capacity]) {
        if (entry.key != null) {
            pb.append_char(' ');
            print_value_buf(entry.key, syms, pb);
        }
    }
    pb.append_char(')');
}

fn void print_array_value_buf(Value* v, SymbolTable* syms, PrintBuf* pb) {
    pb.append_char('[');
    if (v.array_val != null) {
        for (usz vi = 0; vi < v.array_val.length; vi++) {
            if (vi > 0) pb.append_char(' ');
            print_value_buf(v.array_val.items[vi], syms, pb);
        }
    }
    pb.append_char(']');
}

fn void print_tensor_value_buf(Value* v, SymbolTable* syms, PrintBuf* pb) {
    (void)syms;
    TensorVal* tensor = v.tensor_val;
    if (tensor == null) {
        pb.append_str("#<tensor:null>");
        return;
    }
    pb.append_str(tensor.kind == TENSOR_PAYLOAD_CONCRETE ? "#<tensor " : "#<tensor-expr ");
    pb.append_str(tensor_dtype_name(tensor.dtype));
    pb.append_str(" shape [");
    for (usz i = 0; i < tensor.rank; i++) {
        if (i > 0) pb.append_char(' ');
        char[48] dim_buf;
        if (try rendered_dim = io::bprintf(&dim_buf, "%d", tensor.shape[i])) {
            pb.append_str(rendered_dim);
        } else {
            pb.append_str("0");
        }
    }
    pb.append_str("]>");
}

fn void print_instance_value_buf(Value* v, SymbolTable* syms, PrintBuf* pb) {
    if (v.instance_val != null) {
        char[48] ibuf;
        if (try rendered_id = io::bprintf(&ibuf, "%d", v.instance_val.type_id)) {
            pb.append_str("(instance:");
            pb.append_str(rendered_id);
        } else {
            pb.append_str("(instance:");
            pb.append_str("0");
        }
        for (usz fi = 0; fi < v.instance_val.field_count; fi++) {
            pb.append_char(' ');
            print_value_buf(&v.instance_val.fields[fi], syms, pb);
        }
        pb.append_char(')');
    } else {
        pb.append_str("#<instance:null>");
    }
}

fn void print_method_table_value_buf(Value* v, SymbolTable* syms, PrintBuf* pb) {
    if (v.method_table_val == null) {
        pb.append_str("#<dispatch:null>");
        return;
    }

    Primitive* fallback_prim = method_table_fallback_prim(v.method_table_val);
    ZString type_name = method_table_type_descriptor_name(v.method_table_val, syms);
    if (type_name != null) {
        pb.append_str("#<type ");
        pb.append_zstr(type_name);
        pb.append_char('>');
        return;
    }
    if (fallback_prim != null && v.method_table_val.entry_count == 0) {
        pb.append_str("#<primitive ");
        pb.append_zstr((ZString)&fallback_prim.name);
        pb.append_char('>');
        return;
    }

    pb.append_str("#<dispatch ");
    pb.append_str(syms.get_name(v.method_table_val.name));
    if (v.method_table_val.entry_count > 0) {
        char[48] cbuf;
        if (try rendered_count = io::bprintf(&cbuf, "%d", v.method_table_val.entry_count)) {
            pb.append_char(' ');
            pb.append_str(rendered_count);
        } else {
            pb.append_char(' ');
            pb.append_str("0");
        }
        pb.append_str(v.method_table_val.entry_count == 1 ? " method" : " methods");
    }
    if (v.method_table_val.fallback != null) {
        pb.append_str(" +default");
    }
    pb.append_char('>');
}

fn void print_module_value_buf(Value* v, SymbolTable* syms, PrintBuf* pb) {
    if (v.module_val != null) {
        pb.append_str("#<module ");
        pb.append_str(syms.get_name(v.module_val.name));
        pb.append_char('>');
    } else {
        pb.append_str("#<module:null>");
    }
}

fn void print_value_buf(Value* v, SymbolTable* syms, PrintBuf* pb) {
    if (v == null || v.tag == NIL) {
        pb.append_str("nil");
        return;
    }

    switch (v.tag) {
        case INT:
            char[32] ibuf;
            if (try rendered_int = io::bprintf(&ibuf, "%d", v.int_val)) {
                pb.append_str(rendered_int);
            } else {
                pb.append_str("0");
            }
        case DOUBLE:
            char[64] dbuf2;
            char[] dstr2 = double_to_string(v.double_val, &dbuf2);
            pb.append_str(dstr2);
        case FLOAT32:
            char[64] fbuf2;
            char[] fstr2 = double_to_string((double)v.float32_val, &fbuf2);
            pb.append_str(fstr2);
        case BIG_INTEGER:
            print_big_integer_value_buf(v, pb);
        case BIG_FLOAT:
            print_big_float_value_buf(v, pb);
        case BIG_COMPLEX:
            print_big_complex_value_buf(v, pb);
        case COMPLEX128:
            fixed_complex_append_f64(v.complex128_val.real, v.complex128_val.imag, pb);
        case COMPLEX64:
            fixed_complex_append_f64((double)v.complex64_val.real, (double)v.complex64_val.imag, pb);
        case STRING:
            pb.append_char('"');
            pb.append_escaped_string_content(v.str_chars[:v.str_len]);
            pb.append_char('"');
        case SYMBOL:
            pb.append_str(syms.get_name(v.sym_val));
        case CONS:
            print_cons_value_buf(v, syms, pb);
        case VOID:
            pb.append_str("#<void>");
        case CLOSURE:
            print_closure_value_buf(v, syms, pb);
        case CONTINUATION:
            pb.append_str("#<continuation>");
        case PRIMITIVE:
            ZString type_name = primitive_type_descriptor_name(v.prim_val);
            if (type_name != null) {
                pb.append_str("#<type ");
                pb.append_zstr(type_name);
                pb.append_char('>');
            } else {
                pb.append_str("#<primitive ");
                pb.append_zstr((ZString)&v.prim_val.name);
                pb.append_char('>');
            }
        case PARTIAL_PRIM:
            pb.append_str("#<partial>");
        case ERROR:
            pb.append_str("#<error>");
        case HASHMAP:
            print_hashmap_value_buf(v, syms, pb);
        case SET:
            print_set_value_buf(v, syms, pb);
        case FFI_HANDLE:
            pb.append_str("#<ffi-handle:");
            pb.append_zstr((ZString)&v.ffi_val.lib_name);
            pb.append_char('>');
        case ARRAY:
            print_array_value_buf(v, syms, pb);
        case TENSOR:
            print_tensor_value_buf(v, syms, pb);
        case TYPE_INFO:
            pb.append_str("#<type ");
            pb.append_str(syms.get_name(v.type_info_val.name));
            pb.append_char('>');
        case INSTANCE:
            print_instance_value_buf(v, syms, pb);
        case METHOD_TABLE:
            print_method_table_value_buf(v, syms, pb);
        case MODULE:
            print_module_value_buf(v, syms, pb);
        case ITERATOR:
            pb.append_str("#<iterator>");
        case COROUTINE:
            pb.append_str("#<coroutine>");
        case TIME_POINT: {
            char[96] tpb;
            char[] ts = time_point_to_string(v.time_point_val, tpb[..]);
            pb.append_str("#<time-point ");
            pb.append_str(ts);
            pb.append_char('>');
        }
        default:
            pb.append_str("#<unknown>");
    }
}

fn usz print_value_to_buf(Value* v, SymbolTable* syms, char* buf, usz buf_size) {
    if (buf == null || buf_size == 0) return 0;
    PrintBuf pb;
    pb.buf = buf;
    pb.pos = 0;
    pb.capacity = buf_size;
    print_value_buf(v, syms, &pb);
    buf[pb.pos] = 0;
    return pb.pos;
}
