module lisp;

import std::io;

/**
 * Find the best matching method in a method table for the given args.
 * Scoring: Val match (1000) > exact type (100) > subtype (10) > any (1)
 */
fn bool method_constraints_satisfied(MethodEntry* entry, TypeId[] arg_types, Interp* interp) {
    if (entry.sig.constraint_count == 0) return true;

    usz limit = arg_types.len;
    if (limit > entry.sig.param_count) limit = entry.sig.param_count;

    for (usz c = 0; c < entry.sig.constraint_count; c++) {
        TypeId bound = entry.sig.constraints[c].bound_type;
        if (bound == INVALID_TYPE_ID) continue;

        bool constraint_satisfied = false;
        for (usz p = 0; p < limit; p++) {
            if (entry.sig.param_types[p] == INVALID_TYPE_ID && !entry.sig.has_val_literal[p]) {
                TypeId actual = arg_types[p];
                if (actual != INVALID_TYPE_ID && interp.types.is_subtype(actual, bound)) {
                    constraint_satisfied = true;
                    break;
                }
            }
        }
        if (!constraint_satisfied) return false;
    }
    return true;
}

fn bool method_match_score(MethodEntry* entry, Value*[] args, TypeId[] arg_types, Interp* interp, long* score_out) {
    if (entry.sig.param_count != args.len) return false;

    long score = 0;
    usz limit = arg_types.len;
    if (limit > entry.sig.param_count) limit = entry.sig.param_count;
    for (usz j = 0; j < limit; j++) {
        TypeId expected = entry.sig.param_types[j];
        if (entry.sig.has_val_literal[j]) {
            if (args[j] != null && args[j].tag == INT && args[j].int_val == entry.sig.val_literals[j]) {
                score += 1000;
                continue;
            }
            return false;
        }
        if (expected == INVALID_TYPE_ID) {
            score += 1;
            continue;
        }
        TypeId actual = arg_types[j];
        if (actual == INVALID_TYPE_ID) return false;
        if (actual == expected) {
            score += 100;
        } else if (interp.types.is_subtype(actual, expected)) {
            score += 10;
        } else {
            return false;
        }
    }

    if (!method_constraints_satisfied(entry, arg_types, interp)) return false;
    *score_out = score;
    return true;
}

fn Value* find_best_method(MethodTable* mt, Value*[] args, Interp* interp) {
    long best_score = -1;
    Value* best_method = null;
    usz best_count = 0;

    TypeId[] arg_types = mem::new_array(TypeId, args.len);
    defer if (arg_types.len > 0) mem::free(arg_types.ptr);
    for (usz a = 0; a < args.len; a++) {
        arg_types[a] = infer_value_type(args[a], interp);
    }

    for (usz i = 0; i < mt.entry_count; i++) {
        MethodEntry* entry = &mt.entries[i];
        long score = 0;
        if (!method_match_score(entry, args, arg_types, interp, &score)) continue;

        if (score > best_score) {
            best_score = score;
            best_method = entry.implementation;
            best_count = 1;
        } else if (score == best_score) {
            best_count++;
        }
    }

    if (best_count > 1) {
        return raise_error(interp, "ambiguous method call: multiple methods match with equal specificity");
    }

    return best_method;
}

/**
 * Register built-in types in the type registry.
 * Called once during init, after symbol interning.
 */
fn void register_builtin_types(Interp* interp) {
    // Register built-in types with TK_BUILTIN kind
    SymbolId[13] builtin_names = {
        interp.sym_Int, interp.sym_Double, interp.sym_String,
        interp.sym_Symbol, interp.sym_List, interp.sym_Bool,
        interp.sym_Nil, interp.sym_Closure, interp.sym_Array,
        interp.sym_Dict, interp.sym_Any, interp.sym_Iterator,
        interp.sym_TimePoint
    };
    for (usz i = 0; i < 13; i++) {
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
    interp.tid_Int = interp.types.lookup(interp.sym_Int, &interp.symbols);
    interp.tid_Double = interp.types.lookup(interp.sym_Double, &interp.symbols);
    interp.tid_String = interp.types.lookup(interp.sym_String, &interp.symbols);
    interp.tid_Symbol = interp.types.lookup(interp.sym_Symbol, &interp.symbols);
    interp.tid_List = interp.types.lookup(interp.sym_List, &interp.symbols);
    interp.tid_Bool = interp.types.lookup(interp.sym_Bool, &interp.symbols);
    interp.tid_Nil = interp.types.lookup(interp.sym_Nil, &interp.symbols);
    interp.tid_Closure = interp.types.lookup(interp.sym_Closure, &interp.symbols);
    interp.tid_Array = interp.types.lookup(interp.sym_Array, &interp.symbols);
    interp.tid_Dict = interp.types.lookup(interp.sym_Dict, &interp.symbols);
    interp.tid_Any = interp.types.lookup(interp.sym_Any, &interp.symbols);
    interp.tid_Number = interp.types.lookup(interp.sym_Number, &interp.symbols);
    interp.tid_Collection = interp.types.lookup(interp.sym_Collection, &interp.symbols);
    interp.tid_Iterator = interp.types.lookup(interp.sym_Iterator, &interp.symbols);
    interp.tid_TimePoint = interp.types.lookup(interp.sym_TimePoint, &interp.symbols);

    // Set parent chains: Int/Double → Number, List/Array/Dict → Collection, Nil → List
    interp.types.types[(usz)interp.tid_Int].parent = interp.tid_Number;
    interp.types.types[(usz)interp.tid_Double].parent = interp.tid_Number;
    interp.types.types[(usz)interp.tid_List].parent = interp.tid_Collection;
    interp.types.types[(usz)interp.tid_Array].parent = interp.tid_Collection;
    interp.types.types[(usz)interp.tid_Dict].parent = interp.tid_Collection;
    interp.types.types[(usz)interp.tid_Nil].parent = interp.tid_List;  // nil IS the empty list
    interp.types.types[(usz)interp.tid_TimePoint].parent = interp.tid_Any;
}

/**
 * Infer the TypeId for a runtime value.
 */
fn TypeId infer_value_type(Value* v, Interp* interp) {
    if (v == null || v.tag == NIL) return interp.tid_Nil;
    switch (v.tag) {
        case INT:       return interp.tid_Int;
        case DOUBLE:    return interp.tid_Double;
        case STRING:    return interp.tid_String;
        case SYMBOL:    return interp.tid_Symbol;
        case CONS:      return interp.tid_List;
        case CLOSURE:   return interp.tid_Closure;
        case ARRAY:     return interp.tid_Array;
        case HASHMAP:   return interp.tid_Dict;
        case ITERATOR:  return interp.tid_Iterator;
        case TIME_POINT:return interp.tid_TimePoint;
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
        case INT:       return interp.sym_Int;
        case DOUBLE:    return interp.sym_Double;
        case STRING:    return interp.sym_String;
        case SYMBOL:
            // true/false are symbols but semantically Bool
            if (v.sym_val == interp.sym_true || v.sym_val == interp.sym_false) { return interp.sym_Bool; }
            return interp.sym_Symbol;
        case CONS:      return interp.sym_List;
        case CLOSURE:   return interp.sym_Closure;
        case ARRAY:     return interp.sym_Array;
        case HASHMAP:   return interp.sym_Dict;
        case ITERATOR:  return interp.sym_Iterator;
        case TIME_POINT:return interp.sym_TimePoint;
        case COROUTINE:     return interp.symbols.intern("Coroutine");
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
