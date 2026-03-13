module lisp;

import std::io;

/**
 * Register built-in types in the type registry.
 * Called once during init, after symbol interning.
 */
fn void register_builtin_types(Interp* interp) {
    // Register built-in types with TK_BUILTIN kind
    SymbolId[16] builtin_names = {
        interp.sym_Integer, interp.sym_Double, interp.sym_String,
        interp.sym_Symbol, interp.sym_List, interp.sym_Boolean,
        interp.sym_Nil, interp.sym_Void, interp.sym_Closure, interp.sym_Array,
        interp.sym_Dictionary, interp.sym_Set, interp.sym_Any, interp.sym_Iterator,
        interp.sym_TimePoint, interp.sym_Coroutine
    };
    for (usz i = 0; i < 16; i++) {
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
        interp.types.register_type(info, &interp.symbols);
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
    interp.types.register_type(abs_info, &interp.symbols);

    abs_info.name = interp.sym_Collection;
    interp.types.register_type(abs_info, &interp.symbols);

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
    TypeInfo int_alias;
    int_alias.name = interp.sym_Int;
    int_alias.kind = TK_ALIAS;
    int_alias.parent = INVALID_TYPE_ID;
    int_alias.fields = null;
    int_alias.field_count = 0;
    int_alias.type_params = null;
    int_alias.type_param_count = 0;
    int_alias.constraints = null;
    int_alias.constraint_count = 0;
    int_alias.variants = null;
    int_alias.variant_count = 0;
    int_alias.alias_target = interp.tid_Integer;
    interp.types.register_type(int_alias, &interp.symbols);

    TypeInfo bool_alias;
    bool_alias.name = interp.sym_Bool;
    bool_alias.kind = TK_ALIAS;
    bool_alias.parent = INVALID_TYPE_ID;
    bool_alias.fields = null;
    bool_alias.field_count = 0;
    bool_alias.type_params = null;
    bool_alias.type_param_count = 0;
    bool_alias.constraints = null;
    bool_alias.constraint_count = 0;
    bool_alias.variants = null;
    bool_alias.variant_count = 0;
    bool_alias.alias_target = interp.tid_Boolean;
    interp.types.register_type(bool_alias, &interp.symbols);

    TypeInfo dict_alias;
    dict_alias.name = interp.sym_Dict;
    dict_alias.kind = TK_ALIAS;
    dict_alias.parent = INVALID_TYPE_ID;
    dict_alias.fields = null;
    dict_alias.field_count = 0;
    dict_alias.type_params = null;
    dict_alias.type_param_count = 0;
    dict_alias.constraints = null;
    dict_alias.constraint_count = 0;
    dict_alias.variants = null;
    dict_alias.variant_count = 0;
    dict_alias.alias_target = interp.types.lookup(interp.sym_Dictionary, &interp.symbols);
    interp.types.register_type(dict_alias, &interp.symbols);

    interp.tid_Int = interp.types.lookup(interp.sym_Int, &interp.symbols);
    interp.tid_Bool = interp.types.lookup(interp.sym_Bool, &interp.symbols);
    interp.tid_Array = interp.types.lookup(interp.sym_Array, &interp.symbols);
    interp.tid_Dictionary = interp.types.lookup(interp.sym_Dictionary, &interp.symbols);
    interp.tid_Dict = interp.types.lookup(interp.sym_Dict, &interp.symbols);
    interp.tid_Set = interp.types.lookup(interp.sym_Set, &interp.symbols);
    interp.tid_Any = interp.types.lookup(interp.sym_Any, &interp.symbols);
    interp.tid_Number = interp.types.lookup(interp.sym_Number, &interp.symbols);
    interp.tid_Collection = interp.types.lookup(interp.sym_Collection, &interp.symbols);
    interp.tid_Iterator = interp.types.lookup(interp.sym_Iterator, &interp.symbols);
    interp.tid_TimePoint = interp.types.lookup(interp.sym_TimePoint, &interp.symbols);
    interp.tid_Coroutine = interp.types.lookup(interp.sym_Coroutine, &interp.symbols);

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
        case INSTANCE:
            if (v.instance_val != null) return v.instance_val.type_id;
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

/**
 * Format a detailed dispatch error showing method name, arg types, and arity hints.
 */
fn Value* format_dispatch_error(MethodTable* mt, Value*[] args, Interp* interp) {
    char[] name = interp.symbols.get_name(mt.name);

    // Build "Int, String" arg type list
    DString ds;
    ds.init(mem);
    defer ds.free();

    for (usz i = 0; i < args.len; i++) {
        if (i > 0) ds.append_string(", ");
        ds.append_string((String)interp.symbols.get_name(value_type_name(args[i], interp)));
    }
    ZString arg_types = ds.zstr_view();

    // Check if arity mismatch is the issue
    bool has_arity_match = false;
    for (usz i = 0; i < mt.entry_count; i++) {
        if (mt.entries[i].sig.param_count == args.len) { has_arity_match = true; break; }
    }

    char[512] buf;
    char[] msg;
    if (mt.entry_count > 0 && !has_arity_match) {
        // Collect unique arities
        DString arities;
        arities.init(mem);
        defer arities.free();
        usz[8] seen_arities;
        usz seen_count = 0;
        for (usz i = 0; i < mt.entry_count && seen_count < 8; i++) {
            usz pc = mt.entries[i].sig.param_count;
            bool dup = false;
            for (usz s = 0; s < seen_count; s++) { if (seen_arities[s] == pc) { dup = true; break; } }
            if (!dup) {
                seen_arities[seen_count++] = pc;
                if (arities.len() > 0) arities.append_string(", ");
                char[4] nbuf;
                arities.append_string((String)int_to_string((long)pc, &nbuf));
            }
        }
        msg = io::bprintf(&buf, "no matching method '%s': got %d arg(s) (%s), expected %s arg(s)",
            (ZString)name, (int)args.len, arg_types, arities.zstr_view())!!;
    } else {
        msg = io::bprintf(&buf, "no matching method '%s' for (%s)",
            (ZString)name, arg_types)!!;
    }
    return raise_error(interp, msg);
}

/**
 * Format a detailed match error showing type info and missing union variants.
 */
fn Value* format_match_error(Value* scrutinee, Expr* expr, Interp* interp) {
    char[512] buf;
    char[] type_name = interp.symbols.get_name(value_type_name(scrutinee, interp));

    // Check if scrutinee is an Instance of a union type
    if (scrutinee != null && scrutinee.tag == INSTANCE && scrutinee.instance_val != null) {
        TypeId tid = scrutinee.instance_val.type_id;
        TypeInfo* ti = interp.types.get(tid);
        if (ti != null && ti.parent != INVALID_TYPE_ID) {
            TypeInfo* parent = interp.types.get(ti.parent);
            if (parent != null && parent.kind == TK_UNION) {
                DString missing;
                missing.init(mem);
                defer missing.free();
                usz missing_count = 0;
                for (usz v = 0; v < parent.variant_count; v++) {
                    char[] vname = interp.symbols.get_name(parent.variants[v].name);
                    bool covered = false;
                    for (usz c = 0; c < expr.match.clause_count; c++) {
                        Pattern* pat = expr.match.clauses[c].pattern;
                        if (pat.tag == PAT_CONSTRUCTOR &&
                            (uint)pat.constructor_name == (uint)parent.variants[v].name) {
                            covered = true; break;
                        }
                        if (pat.tag == PAT_WILDCARD) { covered = true; break; }
                        if (pat.tag == PAT_VAR) {
                            TypeId vt = interp.types.lookup(pat.var_name, &interp.symbols);
                            if (vt != INVALID_TYPE_ID && (uint)pat.var_name == (uint)parent.variants[v].name) {
                                covered = true; break;
                            }
                        }
                    }
                    if (!covered) {
                        if (missing_count > 0) missing.append_string(", ");
                        missing.append_string((String)vname);
                        missing_count++;
                    }
                }
                if (missing_count > 0) {
                    char[] parent_name = interp.symbols.get_name(parent.name);
                    char[] msg = io::bprintf(&buf, "no pattern matched: %s value '%s', missing variants: %s",
                        (ZString)parent_name, (ZString)type_name, missing.zstr_view())!!;
                    return raise_error(interp, msg);
                }
            }
        }
    }

    // Fallback for non-union types
    char[] msg = io::bprintf(&buf, "no pattern matched for value of type %s", (ZString)type_name)!!;
    return raise_error(interp, msg);
}
