module lisp;

import std::io;

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

    if (zstr_eq(name, "Integer")) return (ZString)"Integer";
    if (zstr_eq(name, "BigInteger")) return (ZString)"BigInteger";
    if (zstr_eq(name, "BigFloat")) return (ZString)"BigFloat";
    if (zstr_eq(name, "BigComplex")) return (ZString)"BigComplex";
    if (zstr_eq(name, "Boolean")) return (ZString)"Boolean";
    if (zstr_eq(name, "Dictionary")) return (ZString)"Dictionary";
    if (zstr_eq(name, "Dict")) return (ZString)"Dictionary";

    if (zstr_eq(name, "Integer") ||
        zstr_eq(name, "BigInteger") ||
        zstr_eq(name, "BigFloat") ||
        zstr_eq(name, "BigComplex") ||
        zstr_eq(name, "Double") ||
        zstr_eq(name, "String") ||
        zstr_eq(name, "Symbol") ||
        zstr_eq(name, "Boolean") ||
        zstr_eq(name, "Nil") ||
        zstr_eq(name, "Void") ||
        zstr_eq(name, "Closure") ||
        zstr_eq(name, "List") ||
        zstr_eq(name, "Array") ||
        zstr_eq(name, "Tensor") ||
        zstr_eq(name, "Dictionary") ||
        zstr_eq(name, "Set") ||
        zstr_eq(name, "Iterator") ||
        zstr_eq(name, "Coroutine") ||
        zstr_eq(name, "TimePoint")) {
        return (ZString)name;
    }
    return null;
}

fn ZString symbol_type_descriptor_name(char* name) {
    if (name == null) return null;

    if (zstr_eq(name, "Integer")) return (ZString)"Integer";
    if (zstr_eq(name, "BigInteger")) return (ZString)"BigInteger";
    if (zstr_eq(name, "BigFloat")) return (ZString)"BigFloat";
    if (zstr_eq(name, "BigComplex")) return (ZString)"BigComplex";
    if (zstr_eq(name, "Boolean")) return (ZString)"Boolean";
    if (zstr_eq(name, "Dictionary")) return (ZString)"Dictionary";
    if (zstr_eq(name, "Dict")) return (ZString)"Dictionary";

    if (zstr_eq(name, "Integer") ||
        zstr_eq(name, "BigInteger") ||
        zstr_eq(name, "BigFloat") ||
        zstr_eq(name, "BigComplex") ||
        zstr_eq(name, "Double") ||
        zstr_eq(name, "String") ||
        zstr_eq(name, "Symbol") ||
        zstr_eq(name, "Boolean") ||
        zstr_eq(name, "Nil") ||
        zstr_eq(name, "Void") ||
        zstr_eq(name, "Closure") ||
        zstr_eq(name, "List") ||
        zstr_eq(name, "Array") ||
        zstr_eq(name, "Tensor") ||
        zstr_eq(name, "Dictionary") ||
        zstr_eq(name, "Set") ||
        zstr_eq(name, "Iterator") ||
        zstr_eq(name, "Coroutine") ||
        zstr_eq(name, "TimePoint") ||
        zstr_eq(name, "Any") ||
        zstr_eq(name, "Number") ||
        zstr_eq(name, "Collection")) {
        return (ZString)name;
    }
    return null;
}

fn Primitive* method_table_fallback_prim(MethodTable* mt) {
    if (mt == null || mt.fallback == null) return null;
    if (mt.fallback.tag != PRIMITIVE || mt.fallback.prim_val == null) return null;
    return mt.fallback.prim_val;
}

fn ZString method_table_type_descriptor_name(MethodTable* mt, SymbolTable* syms) {
    if (mt == null || syms == null) return null;

    ZString name_descriptor = symbol_type_descriptor_name(syms.get_name(mt.name));
    if (name_descriptor != null) return name_descriptor;

    Primitive* fallback_prim = method_table_fallback_prim(mt);
    if (fallback_prim == null) return null;
    return primitive_type_descriptor_name(fallback_prim);
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
    if (v.hashmap_val == null) {
        io::print("}");
        return;
    }
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
    if (v.hashmap_val == null) {
        io::print(")");
        return;
    }
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

fn void print_tensor_value(Value* v, SymbolTable* syms) {
    (void)syms;
    TensorVal* tensor = v.tensor_val;
    if (tensor == null) {
        io::print("#<tensor:null>");
        return;
    }
    io::printf(
        tensor.kind == TENSOR_PAYLOAD_CONCRETE ? "#<tensor %s shape [" : "#<tensor-expr %s shape [",
        (ZString)tensor_dtype_name(tensor.dtype)
    );
    for (usz i = 0; i < tensor.rank; i++) {
        if (i > 0) io::print(" ");
        io::printf("%d", (long)tensor.shape[i]);
    }
    io::print("]>");
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
        ZString type_name = method_table_type_descriptor_name(v.method_table_val, syms);
        if (type_name != null) {
            io::printf("#<type %s>", type_name);
            return;
        }
        if (fallback_prim != null && v.method_table_val.entry_count == 0) {
            io::printf("#<primitive %s>", (ZString)&fallback_prim.name);
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
