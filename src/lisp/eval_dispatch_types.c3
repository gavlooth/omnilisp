module lisp;

import std::io;

fn Value* prim_meta_type_symbol_not_callable(Value*[] args, Env* env, Interp* interp) {
    if (interp == null || interp.last_call_name == INVALID_SYMBOL_ID) {
        return raise_error(interp, "type symbol is not a callable constructor/coercion surface");
    }

    char[160] buf;
    char[] msg = "type symbol is not a callable constructor/coercion surface";
    if (try char[] formatted = io::bprintf(&buf,
            "%s: not a callable constructor/coercion surface",
            (ZString)interp.symbols.get_name(interp.last_call_name))) {
        msg = formatted;
    }
    return raise_error(interp, msg);
}

fn MethodTable* alloc_empty_method_table_bootstrap(SymbolId sym, Value* fallback) {
    MethodTable* mt = (MethodTable*)mem::malloc(MethodTable.sizeof);
    if (mt == null) return null;
    MethodEntry* entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * METHOD_INITIAL_CAPACITY);
    if (entries == null) {
        mem::free(mt);
        return null;
    }
    mt.name = sym;
    mt.entries = entries;
    mt.entry_count = 0;
    mt.capacity = METHOD_INITIAL_CAPACITY;
    mt.fallback = fallback;
    return mt;
}

fn void free_empty_method_table_bootstrap(MethodTable* mt) {
    if (mt == null) return;
    if (mt.entries != null) mem::free(mt.entries);
    mem::free(mt);
}

fn bool bind_non_callable_type_symbol(Interp* interp, SymbolId sym) {
    if (interp == null) return false;
    if (interp.global_env == null) return false;
    if (interp.global_env.lookup(sym) != null) return true;

    char[] name = interp.symbols.get_name(sym);
    Value* prim = make_primitive(interp, name, &prim_meta_type_symbol_not_callable, -1);
    if (prim == null) return false;
    MethodTable* mt = alloc_empty_method_table_bootstrap(sym, prim);
    if (mt == null) return false;

    Value* mt_val = boundary_alloc_value_in_root(interp, true);
    if (mt_val == null) {
        free_empty_method_table_bootstrap(mt);
        return false;
    }
    mt_val.tag = METHOD_TABLE;
    mt_val.method_table_val = mt;
    if (!env_define_with_barrier_checked(interp.global_env, sym, mt_val, interp)) {
        free_empty_method_table_bootstrap(mt);
        mt_val.method_table_val = null;
        return false;
    }
    return true;
}

/**
 * Register built-in types in the type registry.
 * Called once during init, after symbol interning.
 */
fn bool register_builtin_types(Interp* interp) {
    // Register built-in types with TK_BUILTIN kind
    SymbolId[17] builtin_names = {
        interp.sym_Integer, interp.sym_Double, interp.sym_String,
        interp.sym_Symbol, interp.sym_List, interp.sym_Boolean,
        interp.sym_Nil, interp.sym_Void, interp.sym_Closure, interp.sym_Array,
        interp.sym_Dictionary, interp.sym_Set, interp.sym_Any, interp.sym_Iterator,
        interp.sym_TimePoint, interp.sym_Coroutine, interp.sym_Tensor
    };
    for (usz i = 0; i < 17; i++) {
        TypeInfo info;
        info.name = builtin_names[i];
        info.kind = TK_BUILTIN;
        info.parent = INVALID_TYPE_ID;
        info.fields = null;
        info.field_count = 0;
        info.type_params = null;
        info.type_param_count = 0;
        info.constraints = null;
        info.constraint_count = 0;
        info.variants = null;
        info.variant_count = 0;
        info.alias_target = INVALID_TYPE_ID;
        if (interp.types.register_type(info, &interp.symbols) == INVALID_TYPE_ID) return false;
    }

    // Register abstract types: Number, Collection
    TypeInfo abs_info;
    abs_info.kind = TK_ABSTRACT;
    abs_info.parent = INVALID_TYPE_ID;
    abs_info.fields = null;
    abs_info.field_count = 0;
    abs_info.type_params = null;
    abs_info.type_param_count = 0;
    abs_info.constraints = null;
    abs_info.constraint_count = 0;
    abs_info.variants = null;
    abs_info.variant_count = 0;
    abs_info.alias_target = INVALID_TYPE_ID;

    abs_info.name = interp.sym_Number;
    if (interp.types.register_type(abs_info, &interp.symbols) == INVALID_TYPE_ID) return false;

    abs_info.name = interp.sym_Collection;
    if (interp.types.register_type(abs_info, &interp.symbols) == INVALID_TYPE_ID) return false;

    // Cache TypeIds for O(1) dispatch lookups
    interp.tid_Integer = interp.types.lookup(interp.sym_Integer, &interp.symbols);
    interp.tid_Double = interp.types.lookup(interp.sym_Double, &interp.symbols);
    interp.tid_String = interp.types.lookup(interp.sym_String, &interp.symbols);
    interp.tid_Symbol = interp.types.lookup(interp.sym_Symbol, &interp.symbols);
    interp.tid_List = interp.types.lookup(interp.sym_List, &interp.symbols);
    interp.tid_Boolean = interp.types.lookup(interp.sym_Boolean, &interp.symbols);
    interp.tid_Nil = interp.types.lookup(interp.sym_Nil, &interp.symbols);
    interp.tid_Void = interp.types.lookup(interp.sym_Void, &interp.symbols);
    interp.tid_Closure = interp.types.lookup(interp.sym_Closure, &interp.symbols);
    interp.tid_Array = interp.types.lookup(interp.sym_Array, &interp.symbols);
    interp.tid_Dictionary = interp.types.lookup(interp.sym_Dictionary, &interp.symbols);
    interp.tid_Set = interp.types.lookup(interp.sym_Set, &interp.symbols);
    interp.tid_Any = interp.types.lookup(interp.sym_Any, &interp.symbols);
    interp.tid_Number = interp.types.lookup(interp.sym_Number, &interp.symbols);
    interp.tid_Collection = interp.types.lookup(interp.sym_Collection, &interp.symbols);
    interp.tid_Iterator = interp.types.lookup(interp.sym_Iterator, &interp.symbols);
    interp.tid_TimePoint = interp.types.lookup(interp.sym_TimePoint, &interp.symbols);
    interp.tid_Coroutine = interp.types.lookup(interp.sym_Coroutine, &interp.symbols);
    interp.tid_Tensor = interp.types.lookup(interp.sym_Tensor, &interp.symbols);
    if (interp.tid_Integer == INVALID_TYPE_ID || interp.tid_Double == INVALID_TYPE_ID ||
        interp.tid_String == INVALID_TYPE_ID || interp.tid_Symbol == INVALID_TYPE_ID ||
        interp.tid_List == INVALID_TYPE_ID || interp.tid_Boolean == INVALID_TYPE_ID ||
        interp.tid_Nil == INVALID_TYPE_ID || interp.tid_Void == INVALID_TYPE_ID ||
        interp.tid_Closure == INVALID_TYPE_ID || interp.tid_Array == INVALID_TYPE_ID ||
        interp.tid_Dictionary == INVALID_TYPE_ID || interp.tid_Set == INVALID_TYPE_ID ||
        interp.tid_Any == INVALID_TYPE_ID || interp.tid_Number == INVALID_TYPE_ID ||
        interp.tid_Collection == INVALID_TYPE_ID || interp.tid_Iterator == INVALID_TYPE_ID ||
        interp.tid_TimePoint == INVALID_TYPE_ID || interp.tid_Coroutine == INVALID_TYPE_ID ||
        interp.tid_Tensor == INVALID_TYPE_ID) {
        return false;
    }

    // Set parent chains: Integer/Double → Number, List/Array/Dictionary/Set → Collection,
    // Nil → List, and standalone runtime leaf types under Any.
    interp.types.types[(usz)interp.tid_Integer].parent = interp.tid_Number;
    interp.types.types[(usz)interp.tid_Double].parent = interp.tid_Number;
    interp.types.types[(usz)interp.tid_List].parent = interp.tid_Collection;
    interp.types.types[(usz)interp.tid_Array].parent = interp.tid_Collection;
    interp.types.types[(usz)interp.tid_Dictionary].parent = interp.tid_Collection;
    interp.types.types[(usz)interp.tid_Set].parent = interp.tid_Collection;
    interp.types.types[(usz)interp.tid_Nil].parent = interp.tid_List;  // nil IS the empty list
    interp.types.types[(usz)interp.tid_Void].parent = interp.tid_Any;
    interp.types.types[(usz)interp.tid_TimePoint].parent = interp.tid_Any;
    interp.types.types[(usz)interp.tid_Coroutine].parent = interp.tid_Any;
    interp.types.types[(usz)interp.tid_Tensor].parent = interp.tid_Any;

    // Meta/abstract symbols are non-callable, but exposed as first-class
    // descriptor values for canonical printing/introspection surfaces.
    if (!bind_non_callable_type_symbol(interp, interp.sym_Any)) return false;
    if (!bind_non_callable_type_symbol(interp, interp.sym_Number)) return false;
    if (!bind_non_callable_type_symbol(interp, interp.sym_Collection)) return false;
    return true;
}

/**
 * Infer the TypeId for a runtime value.
 */
fn TypeId infer_value_type(Value* v, Interp* interp) {
    if (v == null || v.tag == NIL) return interp.tid_Nil;
    switch (v.tag) {
        case INT:       return interp.tid_Integer;
        case DOUBLE:    return interp.tid_Double;
        case STRING:    return interp.tid_String;
        case SYMBOL:    return interp.tid_Symbol;
        case CONS:      return interp.tid_List;
        case VOID:      return interp.tid_Void;
        case CLOSURE:   return interp.tid_Closure;
        case PRIMITIVE:
            // AOT compiled closures are represented as primitive wrappers.
            if (is_aot_closure_wrapper(v)) return interp.tid_Closure;
            return INVALID_TYPE_ID;
        case ARRAY:     return interp.tid_Array;
        case HASHMAP:   return interp.tid_Dictionary;
        case SET:       return interp.tid_Set;
        case ITERATOR:  return interp.tid_Iterator;
        case TIME_POINT:return interp.tid_TimePoint;
        case COROUTINE: return interp.tid_Coroutine;
        case TENSOR:    return interp.tid_Tensor;
        case INSTANCE:
            if (v.instance_val != null && interp.types.get(v.instance_val.type_id) != null) return v.instance_val.type_id;
            return INVALID_TYPE_ID;
        default:
            return INVALID_TYPE_ID;
    }
}

/**
 * Get the type name symbol for a runtime value (for type-of primitive).
 */
fn SymbolId value_type_name(Value* v, Interp* interp) {
    if (v == null || v.tag == NIL) return interp.sym_Nil;
    switch (v.tag) {
        case INT:       return interp.sym_Integer;
        case DOUBLE:    return interp.sym_Double;
        case STRING:    return interp.sym_String;
        case SYMBOL:
            // true/false are symbols but semantically Boolean
            if (v.sym_val == interp.sym_true || v.sym_val == interp.sym_false) { return interp.sym_Boolean; }
            return interp.sym_Symbol;
        case CONS:      return interp.sym_List;
        case VOID:      return interp.sym_Void;
        case CLOSURE:   return interp.sym_Closure;
        case PRIMITIVE:
            // Preserve closure introspection parity for AOT wrapper closures.
            if (is_aot_closure_wrapper(v)) return interp.sym_Closure;
            return interp.sym_Any;
        case ARRAY:     return interp.sym_Array;
        case HASHMAP:   return interp.sym_Dictionary;
        case SET:       return interp.sym_Set;
        case ITERATOR:  return interp.sym_Iterator;
        case TIME_POINT:return interp.sym_TimePoint;
        case COROUTINE:  return interp.sym_Coroutine;
        case TENSOR:    return interp.sym_Tensor;
        case INSTANCE:
            if (v.instance_val != null) {
                TypeInfo* ti = interp.types.get(v.instance_val.type_id);
                if (ti != null) return ti.name;
            }
            return interp.sym_Any;
        default:
            return interp.sym_Any;
    }
}
