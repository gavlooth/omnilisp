module lisp;


import std::io;
// =============================================================================
// SECTION 8: VALUE PRINTING
// =============================================================================

fn void print_value(Value* v, SymbolTable* syms) {
    if (v == null || v.tag == NIL) {
        io::print("nil");
        return;
    }

    switch (v.tag) {
        case NIL:
            return;
        case INT:
            io::printf("%d", v.int_val);
        case DOUBLE:
            char[64] dbuf;
            char[] dstr = double_to_string(v.double_val, &dbuf);
            dbuf[dstr.len] = 0;
            io::printf("%s", (ZString)&dbuf);
        case FLOAT32:
            char[64] fbuf;
            char[] fstr = double_to_string((double)v.float32_val, &fbuf);
            fbuf[fstr.len] = 0;
            io::printf("%s", (ZString)&fbuf);
        case BIG_INTEGER:
            print_big_integer_value(v);
        case BIG_FLOAT:
            print_big_float_value(v);
        case BIG_COMPLEX:
            print_big_complex_value(v);
        case COMPLEX128:
            char[160] c128buf;
            PrintBuf c128pb;
            c128pb.buf = &c128buf[0];
            c128pb.pos = 0;
            c128pb.capacity = c128buf.len;
            fixed_complex_append_f64(v.complex128_val.real, v.complex128_val.imag, &c128pb);
            c128buf[c128pb.pos] = 0;
            io::printf("%s", (ZString)&c128buf);
        case COMPLEX64:
            char[160] c64buf;
            PrintBuf c64pb;
            c64pb.buf = &c64buf[0];
            c64pb.pos = 0;
            c64pb.capacity = c64buf.len;
            fixed_complex_append_f64((double)v.complex64_val.real, (double)v.complex64_val.imag, &c64pb);
            c64buf[c64pb.pos] = 0;
            io::printf("%s", (ZString)&c64buf);
        case STRING:
            io::print("\"");
            value_print_emit_escaped_string_content(v.str_chars[:v.str_len]);
            io::print("\"");
        case SYMBOL:
            io::printf("%s", (ZString)syms.get_name(v.sym_val));
        case CONS:
            print_cons_value(v, syms);
        case VOID:
            io::print("#<void>");
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
        case TENSOR:
            print_tensor_value(v, syms);
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
            io::print((String)ts);
            io::print(">");
        }
    }
}

fn void println_value(Value* v, SymbolTable* syms) {
    print_value(v, syms);
    io::printn("");
}
