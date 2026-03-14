module lisp;


import std::io;
// =============================================================================
// SECTION 8: VALUE PRINTING
// =============================================================================

fn bool zstr_eq(char* left, char* right) @inline {
    if (left == null || right == null) return left == right;
    usz i = 0;
    while (left[i] != 0 && right[i] != 0) {
        if (left[i] != right[i]) return false;
        i++;
    }
    return left[i] == right[i];
}

fn ZString primitive_type_descriptor_name(Primitive* prim) {
    if (prim == null) return null;
    char* name = &prim.name;

    // User-defined type constructors share prim_type_constructor.
    if (prim.func == &prim_type_constructor) return (ZString)name;

    if (zstr_eq(name, "Int")) return (ZString)"Integer";
    if (zstr_eq(name, "Bool")) return (ZString)"Boolean";
    if (zstr_eq(name, "Dict")) return (ZString)"Dictionary";

    if (zstr_eq(name, "Integer") ||
        zstr_eq(name, "Double") ||
        zstr_eq(name, "String") ||
        zstr_eq(name, "Symbol") ||
        zstr_eq(name, "Boolean") ||
        zstr_eq(name, "Nil") ||
        zstr_eq(name, "Void") ||
        zstr_eq(name, "Closure") ||
        zstr_eq(name, "List") ||
        zstr_eq(name, "Array") ||
        zstr_eq(name, "Dictionary") ||
        zstr_eq(name, "Set") ||
        zstr_eq(name, "Iterator") ||
        zstr_eq(name, "Coroutine") ||
        zstr_eq(name, "TimePoint")) {
        return (ZString)name;
    }
    return null;
}

fn Primitive* method_table_fallback_prim(MethodTable* mt) {
    if (mt == null || mt.entry_count != 0 || mt.fallback == null) return null;
    if (mt.fallback.tag != PRIMITIVE || mt.fallback.prim_val == null) return null;
    return mt.fallback.prim_val;
}

fn void print_cons_value(Value* v, SymbolTable* syms) {
    io::print("(");
    print_value(v.cons_val.car, syms);
    Value* rest = v.cons_val.cdr;
    while (is_cons(rest)) {
        io::print(" ");
        print_value(rest.cons_val.car, syms);
        rest = rest.cons_val.cdr;
    }
    if (!is_nil(rest)) {
        io::print(" . ");
        print_value(rest, syms);
    }
    io::print(")");
}

fn void print_closure_value(Value* v, SymbolTable* syms) {
    io::print("#<λ");
    if ((uint)v.closure_val.name != 0) {
        io::printf(" %s", (ZString)syms.get_name(v.closure_val.name));
    }
    io::print(" (");
    for (usz i = 0; i < v.closure_val.param_count; i++) {
        if (i > 0) io::print(" ");
        io::printf("%s", (ZString)syms.get_name(v.closure_val.params[i]));
    }
    if (v.closure_val.has_rest) {
        if (v.closure_val.param_count > 0) io::print(" ");
        io::printf(".. %s", (ZString)syms.get_name(v.closure_val.rest_param));
    }
    io::print(")>");
}

fn void print_hashmap_value(Value* v, SymbolTable* syms) {
    io::print("{");
    uint printed = 0;
    for (uint hi = 0; hi < v.hashmap_val.capacity; hi++) {
        if (v.hashmap_val.entries[hi].key != null) {
            if (printed > 0) io::print(" ");
            print_value(v.hashmap_val.entries[hi].key, syms);
            io::print(" ");
            print_value(v.hashmap_val.entries[hi].value, syms);
            printed++;
        }
    }
    io::print("}");
}

fn void print_set_value(Value* v, SymbolTable* syms) {
    io::print("(Set");
    uint printed = 0;
    for (uint hi = 0; hi < v.hashmap_val.capacity; hi++) {
        if (v.hashmap_val.entries[hi].key != null) {
            io::print(" ");
            print_value(v.hashmap_val.entries[hi].key, syms);
            printed++;
        }
    }
    if (printed == 0) {
        io::print(")");
        return;
    }
    io::print(")");
}

fn void print_array_value(Value* v, SymbolTable* syms) {
    io::print("[");
    if (v.array_val != null) {
        for (usz vi = 0; vi < v.array_val.length; vi++) {
            if (vi > 0) io::print(" ");
            print_value(v.array_val.items[vi], syms);
        }
    }
    io::print("]");
}

fn void print_instance_value(Value* v, SymbolTable* syms) {
    if (v.instance_val != null) {
        io::print("(");
        io::printf("instance:%d", v.instance_val.type_id);
        for (usz fi = 0; fi < v.instance_val.field_count; fi++) {
            io::print(" ");
            print_value(&v.instance_val.fields[fi], syms);
        }
        io::print(")");
    } else {
        io::print("#<instance:null>");
    }
}

fn void print_method_table_value(Value* v, SymbolTable* syms) {
    if (v.method_table_val != null) {
        Primitive* fallback_prim = method_table_fallback_prim(v.method_table_val);
        if (fallback_prim != null) {
            ZString type_name = primitive_type_descriptor_name(fallback_prim);
            if (type_name != null) {
                io::printf("#<type %s>", type_name);
            } else {
                io::printf("#<primitive %s>", (ZString)&fallback_prim.name);
            }
            return;
        }
        io::printf("#<dispatch %s", (ZString)syms.get_name(v.method_table_val.name));
        if (v.method_table_val.entry_count > 0) {
            io::printf(" %d method%s", v.method_table_val.entry_count,
                v.method_table_val.entry_count == 1 ? "" : "s");
        }
        if (v.method_table_val.fallback != null) {
            io::print(" +default");
        }
        io::print(">");
    } else {
        io::print("#<dispatch:null>");
    }
}

fn void print_module_value(Value* v, SymbolTable* syms) {
    if (v.module_val != null) {
        io::printf("#<module %s>", (ZString)syms.get_name(v.module_val.name));
    } else {
        io::print("#<module:null>");
    }
}

fn void print_value(Value* v, SymbolTable* syms) {
    if (v == null || v.tag == NIL) {
        io::print("nil");
        return;
    }

    switch (v.tag) {
        case INT:
            io::printf("%d", v.int_val);
        case DOUBLE:
            char[64] dbuf;
            char[] dstr = double_to_string(v.double_val, &dbuf);
            dbuf[dstr.len] = 0;
            io::printf("%s", (ZString)&dbuf);
        case STRING:
            io::printf("\"%s\"", (ZString)v.str_chars);
        case SYMBOL:
            io::printf("%s", (ZString)syms.get_name(v.sym_val));
        case CONS:
            print_cons_value(v, syms);
        case VOID:
            io::print("(Void)");
        case CLOSURE:
            print_closure_value(v, syms);
        case CONTINUATION:
            io::print("#<continuation>");
        case PRIMITIVE:
            ZString type_name = primitive_type_descriptor_name(v.prim_val);
            if (type_name != null) {
                io::printf("#<type %s>", type_name);
            } else {
                io::printf("#<primitive %s>", (ZString)&v.prim_val.name);
            }
        case PARTIAL_PRIM:
            io::printf("#<partial %d more>", v.partial_val.remaining);
        case ERROR:
            io::printf("#<error: %s>", (ZString)v.str_chars);
        case HASHMAP:
            print_hashmap_value(v, syms);
        case SET:
            print_set_value(v, syms);
        case FFI_HANDLE:
            io::printf("#<ffi-handle:%s>", (ZString)&v.ffi_val.lib_name);
        case ARRAY:
            print_array_value(v, syms);
        case TYPE_INFO:
            io::printf("#<type %s>", (ZString)syms.get_name(v.type_info_val.name));
        case INSTANCE:
            print_instance_value(v, syms);
        case METHOD_TABLE:
            print_method_table_value(v, syms);
        case MODULE:
            print_module_value(v, syms);
        case ITERATOR:
            io::print("#<iterator>");
        case COROUTINE:
            io::print("#<coroutine>");
        case TIME_POINT: {
            char[96] tpb;
            char[] ts = time_point_to_string(v.time_point_val, tpb[..]);
            io::print("#<time-point ");
            io::print(ts);
            io::print(">");
        }
        default:
            io::print("#<unknown>");
    }
}

fn void println_value(Value* v, SymbolTable* syms) {
    print_value(v, syms);
    io::printn("");
}

// =============================================================================
// print_value_to_buf — capture print_value output to a char buffer
// =============================================================================

struct PrintBuf {
    char* buf;
    usz pos;
    usz capacity;
}

fn void PrintBuf.append_str(PrintBuf* self, char[] s) {
    for (usz i = 0; i < s.len && self.pos < self.capacity - 1; i++) {
        self.buf[self.pos++] = s[i];
    }
}

fn void PrintBuf.append_char(PrintBuf* self, char c) {
    if (self.pos < self.capacity - 1) {
        self.buf[self.pos++] = c;
    }
}

fn void PrintBuf.append_zstr(PrintBuf* self, ZString zs) {
    char* p = (char*)zs;
    while (*p != 0 && self.pos < self.capacity - 1) {
        self.buf[self.pos++] = *p;
        p++;
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
    uint hprinted = 0;
    for (uint hi = 0; hi < v.hashmap_val.capacity; hi++) {
        if (v.hashmap_val.entries[hi].key != null) {
            if (hprinted > 0) pb.append_char(' ');
            print_value_buf(v.hashmap_val.entries[hi].key, syms, pb);
            pb.append_char(' ');
            print_value_buf(v.hashmap_val.entries[hi].value, syms, pb);
            hprinted++;
        }
    }
    pb.append_char('}');
}

fn void print_set_value_buf(Value* v, SymbolTable* syms, PrintBuf* pb) {
    pb.append_str("(Set");
    for (uint hi = 0; hi < v.hashmap_val.capacity; hi++) {
        if (v.hashmap_val.entries[hi].key != null) {
            pb.append_char(' ');
            print_value_buf(v.hashmap_val.entries[hi].key, syms, pb);
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

fn void print_instance_value_buf(Value* v, SymbolTable* syms, PrintBuf* pb) {
    if (v.instance_val != null) {
        char[48] ibuf;
        char[] id = io::bprintf(&ibuf, "%d", v.instance_val.type_id)!!;
        pb.append_str("(instance:");
        pb.append_str(id);
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
    if (fallback_prim != null) {
        ZString type_name = primitive_type_descriptor_name(fallback_prim);
        if (type_name != null) {
            pb.append_str("#<type ");
            pb.append_zstr(type_name);
            pb.append_char('>');
        } else {
            pb.append_str("#<primitive ");
            pb.append_zstr((ZString)&fallback_prim.name);
            pb.append_char('>');
        }
        return;
    }

    pb.append_str("#<dispatch ");
    pb.append_str(syms.get_name(v.method_table_val.name));
    if (v.method_table_val.entry_count > 0) {
        char[48] cbuf;
        char[] count = io::bprintf(&cbuf, "%d", (int)v.method_table_val.entry_count)!!;
        pb.append_char(' ');
        pb.append_str(count);
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
            char[] is = io::bprintf(&ibuf, "%d", v.int_val)!!;
            pb.append_str(is);
        case DOUBLE:
            char[64] dbuf2;
            char[] dstr2 = double_to_string(v.double_val, &dbuf2);
            pb.append_str(dstr2);
        case STRING:
            pb.append_char('"');
            pb.append_zstr((ZString)v.str_chars);
            pb.append_char('"');
        case SYMBOL:
            pb.append_str(syms.get_name(v.sym_val));
        case CONS:
            print_cons_value_buf(v, syms, pb);
        case VOID:
            pb.append_str("(Void)");
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
    PrintBuf pb;
    pb.buf = buf;
    pb.pos = 0;
    pb.capacity = buf_size;
    print_value_buf(v, syms, &pb);
    buf[pb.pos] = 0;
    return pb.pos;
}
