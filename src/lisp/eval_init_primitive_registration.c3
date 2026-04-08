module lisp;

// Primitive registration table entry
struct PrimReg {
    char[] name;
    PrimitiveFn func;
    int arity;
}

fn bool register_prim_table(Interp* interp, PrimReg[] table) {
    foreach (&r : table) {
        if (!register_prim(interp, r.name, r.func, r.arity)) return false;
    }
    return true;
}

fn bool register_dispatched_prim_table(Interp* interp, PrimReg[] table) {
    foreach (&r : table) {
        if (!register_dispatched_prim(interp, r.name, r.func, r.arity)) return false;
    }
    return true;
}

fn bool register_language_constants(Interp* interp) {
    Value* true_val = make_symbol(interp, interp.sym_true);
    Value* false_val = make_symbol(interp, interp.sym_false);
    Value* nil_val = make_nil(interp);
    if (!env_define_with_barrier_checked(interp.global_env, interp.sym_true, true_val, interp)) return false;
    if (!env_define_with_barrier_checked(interp.global_env, interp.sym_false, false_val, interp)) return false;
    SymbolId sym_nil = interp.symbols.intern("nil");
    return env_define_with_barrier_checked(interp.global_env, sym_nil, nil_val, interp);
}

fn bool register_dispatched_primitives(Interp* interp) {
    const DISPATCHED_PRIM_COUNT = 43;
    PrimReg[DISPATCHED_PRIM_COUNT] dispatched_prims = {
        // Arithmetic
        { "+", &prim_add, 2 }, { "-", &prim_sub, 2 }, { "*", &prim_mul, 2 },
        { "/", &prim_div, 2 }, { "%", &prim_mod, 2 },
        // Comparison
        { "=", &prim_eq, 2 }, { "<", &prim_lt, 2 }, { ">", &prim_gt, 2 },
        { "<=", &prim_le, 2 }, { ">=", &prim_ge, 2 },
        // Collections
        { "length", &prim_length, 1 }, { "ref", &prim_ref, 2 },
        { "set!", &prim_set_bang, 3 },
        { "push!", &prim_push, 2 }, { "keys", &prim_keys, 1 },
        { "values", &prim_values, 1 }, { "has?", &prim_has, 2 },
        { "remove!", &prim_remove, 2 },
        // Sorting
        { "sort", &prim_sort, 1 }, { "sort-by", &prim_sort_by, 2 },
        // String ops (extensible for user-defined string-like types)
        { "string-append", &prim_string_append, -1 },
        { "string-contains?", &prim_string_contains, 2 },
        { "string-upcase", &prim_string_upcase_unicode, 1 },
        { "string-downcase", &prim_string_downcase_unicode, 1 },
        { "string-casefold", &prim_string_casefold_unicode, 1 },
        { "string-titlecase", &prim_string_titlecase_unicode, 1 },
        // Math (extensible for user-defined numeric types)
        { "abs", &prim_abs, 1 }, { "floor", &prim_floor, 1 },
        { "ceiling", &prim_ceiling, 1 }, { "round", &prim_round, 1 },
        { "truncate", &prim_truncate, 1 }, { "sqrt", &prim_sqrt, 1 },
        { "min", &prim_min, 2 }, { "max", &prim_max, 2 },
        // Callable type symbols / constructors
        { "Integer", &prim_ctor_int, 1 },
        { "Double", &prim_ctor_double, 1 },
        { "String", &prim_ctor_string, 1 }, { "Symbol", &prim_ctor_symbol, 1 },
        { "Boolean", &prim_ctor_bool, 1 },
        { "Nil", &prim_ctor_nil, 1 }, { "Void", &prim_ctor_void, -1 },
        { "Closure", &prim_ctor_closure, 1 }, { "Iterator", &prim_ctor_iterator, 1 },
    };
    $assert(dispatched_prims.len == DISPATCHED_PRIM_COUNT);
    return register_dispatched_prim_table(interp, dispatched_prims[0:dispatched_prims.len]);
}
