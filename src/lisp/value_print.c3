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
