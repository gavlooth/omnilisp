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

        case DOUBLE:
            self.emit("aot::make_double(");
            char[64] dbuf;
            if (try rendered_double = io::bprintf(&dbuf, "%.17g", v.double_val)) {
                self.emit(rendered_double);
            } else {
                self.emit("0.0");
            }
            self.emit(")");

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
                } else {
                    self.emit_char(c);
                }
            }
            self.emit("\")");
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

        case SYMBOL:
            self.emit("aot::make_symbol(\"");
            char[] name = self.interp.symbols.get_name(v.sym_val);
            self.emit_escaped(name);
            self.emit("\")");

        case CONS:
            self.emit("aot::cons(");
            self.compile_literal(v.cons_val.car);
            self.emit(", ");
            self.compile_literal(v.cons_val.cdr);
            self.emit(")");

        default:
            self.emit("aot::make_nil()");
    }
}

fn void Compiler.compile_quote(Compiler* self, Value* datum) {
    // Quote compiles to literal value construction
    self.compile_literal(datum);
}
