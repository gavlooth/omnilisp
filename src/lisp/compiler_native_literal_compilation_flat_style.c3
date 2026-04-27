module lisp;

import std::core::mem;
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

        case HASHMAP:
            self.compile_hashmap_literal(v.hashmap_val);
            break;

        case SET:
            self.compile_set_literal(v.hashmap_val);
            break;

        case ARRAY:
            self.compile_array_literal(v.array_val);
            break;

        case PRIMITIVE:
            self.compile_primitive_literal(v.prim_val);
            break;

        case CLOSURE:
            if (self.compile_global_value_literal(v)) break;
            self.set_compile_error("compiler: cannot lower callable literal value");
            self.emit("aot::make_nil()");
            break;

        case CONTINUATION:
        case PARTIAL_PRIM:
            self.set_compile_error("compiler: cannot lower callable literal value");
            self.emit("aot::make_nil()");
            break;

        case ERROR:
            self.set_compile_error("compiler: cannot lower error literal value");
            self.emit("aot::make_nil()");
            break;

        case FFI_HANDLE:
        case TYPE_INFO:
        case INSTANCE:
        case METHOD_TABLE:
        case MODULE:
        case ITERATOR:
        case COROUTINE:
        case TENSOR:
            self.set_compile_error("compiler: cannot lower runtime object literal value");
            self.emit("aot::make_nil()");
            break;

        default:
            self.set_compile_error("compiler: cannot lower unsupported literal value");
            self.emit("aot::make_nil()");
            break;
    }
}

fn void Compiler.compile_quote(Compiler* self, Value* datum) {
    // Quote compiles to literal value construction
    self.compile_literal(datum);
}

fn void Compiler.compile_hashmap_literal(Compiler* self, HashMap* map) {
    if (map == null || !hashmap_storage_valid(map)) {
        self.set_compile_error("compiler: dictionary backing entries are missing");
        self.emit("aot::make_nil()");
        return;
    }

    uint[] slots = hashmap_sorted_slots(map, self.interp);
    if (slots.ptr == null && map.count > 0) {
        self.set_compile_error("compiler: failed to allocate dictionary literal order");
        self.emit("aot::make_nil()");
        return;
    }
    defer if (slots.ptr != null) mem::free(slots.ptr);

    self.emit("aot::dict_from_args(");
    for (usz i = 0; i < slots.len; i++) {
        uint slot = slots[i];
        self.emit("aot::cons(");
        self.compile_literal(map.entries[slot].key);
        self.emit(", aot::cons(");
        self.compile_literal(map.entries[slot].value);
        self.emit(", ");
    }
    self.emit("aot::make_nil()");
    for (usz i = 0; i < slots.len; i++) {
        self.emit("))");
    }
    self.emit(")");
}

fn void Compiler.compile_set_literal(Compiler* self, HashMap* map) {
    if (map == null || !hashmap_storage_valid(map)) {
        self.set_compile_error("compiler: set backing entries are missing");
        self.emit("aot::make_nil()");
        return;
    }

    uint[] slots = hashmap_sorted_slots(map, self.interp);
    if (slots.ptr == null && map.count > 0) {
        self.set_compile_error("compiler: failed to allocate set literal order");
        self.emit("aot::make_nil()");
        return;
    }
    defer if (slots.ptr != null) mem::free(slots.ptr);

    self.emit("aot::set_from_args(");
    for (usz i = 0; i < slots.len; i++) {
        uint slot = slots[i];
        self.emit("aot::cons(");
        self.compile_literal(map.entries[slot].key);
        self.emit(", ");
    }
    self.emit("aot::make_nil()");
    for (usz i = 0; i < slots.len; i++) {
        self.emit(")");
    }
    self.emit(")");
}

fn void Compiler.compile_array_literal(Compiler* self, Array* array) {
    if (array == null || array.items == null) {
        self.set_compile_error("compiler: array backing items are missing");
        self.emit("aot::make_nil()");
        return;
    }

    self.emit("aot::array_from_args(");
    for (usz i = 0; i < array.length; i++) {
        self.emit("aot::cons(");
        self.compile_literal(array.items[i]);
        self.emit(", ");
    }
    self.emit("aot::make_nil()");
    for (usz i = 0; i < array.length; i++) {
        self.emit(")");
    }
    self.emit(")");
}

fn void Compiler.compile_primitive_literal(Compiler* self, Primitive* prim) {
    if (prim == null || prim.func == null) {
        self.set_compile_error("compiler: primitive literal payload is missing");
        self.emit("aot::make_nil()");
        return;
    }
    if (prim.user_data != null || prim.user_data_copy != null || prim.user_data_finalizer != null) {
        self.set_compile_error("compiler: cannot lower opaque primitive literal value");
        self.emit("aot::make_nil()");
        return;
    }

    usz name_len = 0;
    while (name_len < prim.name.len && prim.name[name_len] != 0) {
        name_len++;
    }
    if (name_len == 0 || name_len >= prim.name.len) {
        self.set_compile_error("compiler: primitive literal name is invalid");
        self.emit("aot::make_nil()");
        return;
    }

    self.emit("aot::lookup_prim(\"");
    self.emit_escaped(prim.name[:name_len]);
    self.emit("\")");
}

fn bool Compiler.compile_global_value_literal(Compiler* self, Value* value) {
    SymbolId global_name = INVALID_SYMBOL_ID;
    if (!compiler_find_global_value_symbol(self.interp, value, &global_name)) return false;

    self.emit_symbol_name(global_name);
    return true;
}
