module lisp;

import std::io;
import main;
// =============================================================================
// SECTION: NATIVE LITERAL/QUOTE COMPILATION (flat style)
// =============================================================================

fn void Compiler.compile_literal(Compiler* self, Value* v) {
    if (v == null || v.tag == NIL) {
        self.emit("aot::make_nil()");
        return;
    }

    switch (v.tag) {
        case INT:
            self.emit("aot::make_int(");
            self.emit_int(v.int_val);
            self.emit(")");
            break;

        case DOUBLE:
            self.emit("aot::make_double(");
            char[64] dbuf;
            if (try rendered_double = io::bprintf(&dbuf, "%.17g", v.double_val)) {
                self.emit(rendered_double);
            } else {
                self.emit("0.0");
            }
            self.emit(")");
            break;

        case FLOAT32:
            self.emit("aot::make_float32((float)");
            char[64] fbuf;
            if (try rendered_float = io::bprintf(&fbuf, "%.9g", (double)v.float32_val)) {
                self.emit(rendered_float);
            } else {
                self.emit("0.0");
            }
            self.emit(")");
            break;

        case COMPLEX128:
            self.emit("aot::make_complex128(");
            char[64] c128rbuf;
            if (try rendered_c128r = io::bprintf(&c128rbuf, "%.17g", v.complex128_val.real)) {
                self.emit(rendered_c128r);
            } else {
                self.emit("0.0");
            }
            self.emit(", ");
            char[64] c128ibuf;
            if (try rendered_c128i = io::bprintf(&c128ibuf, "%.17g", v.complex128_val.imag)) {
                self.emit(rendered_c128i);
            } else {
                self.emit("0.0");
            }
            self.emit(")");
            break;

        case COMPLEX64:
            self.emit("aot::make_complex64((float)");
            char[64] c64rbuf;
            if (try rendered_c64r = io::bprintf(&c64rbuf, "%.9g", (double)v.complex64_val.real)) {
                self.emit(rendered_c64r);
            } else {
                self.emit("0.0");
            }
            self.emit(", (float)");
            char[64] c64ibuf;
            if (try rendered_c64i = io::bprintf(&c64ibuf, "%.9g", (double)v.complex64_val.imag)) {
                self.emit(rendered_c64i);
            } else {
                self.emit("0.0");
            }
            self.emit(")");
            break;

        case STRING:
            self.emit("aot::make_string(\"");
            // Emit string with escaping
            for (usz i = 0; i < v.str_len; i++) {
                char c = v.str_chars[i];
                if (c == '"') {
                    self.emit("\\\"");
                } else if (c == '\\') {
                    self.emit("\\\\");
                } else if (c == '\n') {
                    self.emit("\\n");
                } else if (c == '\t') {
                    self.emit("\\t");
                } else if (c == '\0') {
                    self.emit("\\0");
                } else {
                    self.emit_char(c);
                }
            }
            self.emit("\")");
            break;
        case TIME_POINT:
            self.emit("aot::make_time_point(");
            self.emit_int((long)(int)v.time_point_val.kind);
            self.emit(", ");
            self.emit_int(v.time_point_val.year);
            self.emit(", ");
            self.emit_int(v.time_point_val.month);
            self.emit(", ");
            self.emit_int(v.time_point_val.day);
            self.emit(", ");
            self.emit_int(v.time_point_val.hour);
            self.emit(", ");
            self.emit_int(v.time_point_val.minute);
            self.emit(", ");
            self.emit_int(v.time_point_val.second);
            self.emit(", ");
            self.emit_int(v.time_point_val.microsecond);
            self.emit(", ");
            self.emit_int(v.time_point_val.tz_offset);
            self.emit(")");
            break;

        case SYMBOL:
            self.emit("aot::make_symbol(\"");
            char[] name = self.interp.symbols.get_name(v.sym_val);
            self.emit_escaped(name);
            self.emit("\")");
            break;

        case CONS:
            self.emit("aot::cons(");
            self.compile_literal(v.cons_val.car);
            self.emit(", ");
            self.compile_literal(v.cons_val.cdr);
            self.emit(")");
            break;

        default:
            self.emit("aot::make_nil()");
            break;
    }
}

fn void Compiler.compile_quote(Compiler* self, Value* datum) {
    // Quote compiles to literal value construction
    self.compile_literal(datum);
}
