module lisp;

import std::io;

const usz DISPATCH_INLINE_ARG_CAPACITY = 8;
const usz DISPATCH_INLINE_BINDING_CAPACITY = 8;

enum DispatchMatchFailureReason : char {
    DISPATCH_MATCH_FAIL_NONE,
    DISPATCH_MATCH_FAIL_ARITY,
    DISPATCH_MATCH_FAIL_VALUE_LITERAL,
    DISPATCH_MATCH_FAIL_TYPE,
    DISPATCH_MATCH_FAIL_CONSTRAINT
}

struct DispatchMatchBreakdown {
    bool matched;
    long score_total;
    long score_value_literal;
    long score_exact;
    long score_widen;
    long score_subtype;
    long score_any;
    DispatchMatchFailureReason failure_reason;
    usz failure_arg_index;
}

fn bool dispatch_text_equals(char[] a, char[] b) @inline {
    if (a.len != b.len) return false;
    for (usz i = 0; i < a.len; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

fn bool dispatch_numeric_widening_match(TypeId actual, TypeId expected, Interp* interp) @inline {
    // Numeric promotion policy (dispatch-only): Int can satisfy Double-typed methods.
    return actual == interp.tid_Int && expected == interp.tid_Double;
}

fn bool dispatch_value_literal_matches(ValueLiteralKey* literal, Value* arg, Interp* interp) @inline {
    switch (literal.tag) {
        case NIL:
            // `^(Value false)` compiles to a NIL literal key because runtime `false` resolves to nil.
            if ((uint)literal.sym_value == (uint)interp.sym_false) return arg == null || arg.tag == NIL;
            return arg != null && arg.tag == NIL;
        case INT:
            return arg != null && arg.tag == INT && arg.int_val == literal.int_value;
        case SYMBOL:
            return arg != null && arg.tag == SYMBOL && (uint)arg.sym_val == (uint)literal.sym_value;
        case STRING: {
            if (arg == null || arg.tag != STRING) return false;
            char[] expected = interp.symbols.get_name(literal.sym_value);
            return dispatch_text_equals(arg.str_chars[:arg.str_len], expected);
        }
        default:
            return false;
    }
}

/**
 * Find the best matching method in a method table for the given args.
 * Scoring: Value-literal match (1000) > exact type (100) > subtype (10) > any (1)
 */
fn bool method_constraints_satisfied(MethodEntry* entry, TypeId[] arg_types, Interp* interp) {
    usz limit = arg_types.len;
    if (limit > entry.sig.param_count) limit = entry.sig.param_count;

    SymbolId[] bound_syms = {};
    TypeId[] bound_types = {};
    SymbolId[DISPATCH_INLINE_BINDING_CAPACITY] bound_syms_inline;
    TypeId[DISPATCH_INLINE_BINDING_CAPACITY] bound_types_inline;
    SymbolId* bound_syms_storage = null;
    TypeId* bound_types_storage = null;
    usz bound_count = 0;

    if (entry.sig.param_count > 0) {
        if (entry.sig.param_count <= DISPATCH_INLINE_BINDING_CAPACITY) {
            bound_syms = bound_syms_inline[:entry.sig.param_count];
            bound_types = bound_types_inline[:entry.sig.param_count];
        } else {
            bound_syms_storage = (SymbolId*)mem::malloc(SymbolId.sizeof * entry.sig.param_count);
            bound_types_storage = (TypeId*)mem::malloc(TypeId.sizeof * entry.sig.param_count);
            if (bound_syms_storage == null || bound_types_storage == null) {
                if (bound_syms_storage != null) mem::free(bound_syms_storage);
                if (bound_types_storage != null) mem::free(bound_types_storage);
                return false;
            }
            bound_syms = bound_syms_storage[:entry.sig.param_count];
            bound_types = bound_types_storage[:entry.sig.param_count];
        }
    }
    defer if (bound_syms_storage != null) mem::free(bound_syms_storage);
    defer if (bound_types_storage != null) mem::free(bound_types_storage);

    // Unify repeated method type variables (e.g., (^T x) (^T y) requires same runtime type).
    for (usz p = 0; p < limit; p++) {
        if (entry.sig.has_val_literal[p]) continue;
        SymbolId type_var = entry.sig.param_type_vars[p];
        if (type_var == (SymbolId)0) continue;

        TypeId actual = arg_types[p];
        if (actual == INVALID_TYPE_ID) return false;

        bool seen = false;
        for (usz i = 0; i < bound_count; i++) {
            if ((uint)bound_syms[i] != (uint)type_var) continue;
            seen = true;
            if (bound_types[i] != actual) return false;
            break;
        }
        if (!seen) {
            bound_syms[bound_count] = type_var;
            bound_types[bound_count] = actual;
            bound_count++;
        }
    }

    if (entry.sig.constraint_count == 0) return true;

    for (usz c = 0; c < entry.sig.constraint_count; c++) {
        SymbolId constrained_param = entry.sig.constraints[c].param_sym;
        TypeId bound = entry.sig.constraints[c].bound_type;
        if (bound == INVALID_TYPE_ID) continue;

        // Prefer explicit type-variable binding when this constraint key was unified.
        for (usz b = 0; b < bound_count; b++) {
            if ((uint)bound_syms[b] != (uint)constrained_param) continue;
            TypeId inferred = bound_types[b];
            if (inferred == bound || interp.types.is_subtype(inferred, bound)) {
                bound = INVALID_TYPE_ID;
                break;
            }
            return false;
        }
        if (bound == INVALID_TYPE_ID) continue;

        // Legacy fallback: if a constraint key is not explicitly bound to a single param,
        // treat it as existential across untyped parameters.
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
    DispatchMatchBreakdown breakdown = dispatch_match_breakdown(entry, args, arg_types, interp);
    if (!breakdown.matched) return false;
    *score_out = breakdown.score_total;
    return true;
}

fn DispatchMatchBreakdown dispatch_match_breakdown(
    MethodEntry* entry,
    Value*[] args,
    TypeId[] arg_types,
    Interp* interp
) {
    DispatchMatchBreakdown out = {};
    out.failure_reason = DISPATCH_MATCH_FAIL_NONE;
    out.failure_arg_index = usz.max;

    if (entry == null) {
        out.failure_reason = DISPATCH_MATCH_FAIL_TYPE;
        return out;
    }
    if (entry.sig.param_count != args.len) {
        out.failure_reason = DISPATCH_MATCH_FAIL_ARITY;
        return out;
    }

    usz limit = arg_types.len;
    if (limit > entry.sig.param_count) limit = entry.sig.param_count;
    for (usz j = 0; j < limit; j++) {
        TypeId expected = entry.sig.param_types[j];
        if (entry.sig.has_val_literal[j]) {
            if (dispatch_value_literal_matches(&entry.sig.val_literals[j], args[j], interp)) {
                out.score_value_literal++;
                out.score_total += 1000;
                continue;
            }
            out.failure_reason = DISPATCH_MATCH_FAIL_VALUE_LITERAL;
            out.failure_arg_index = j;
            return out;
        }
        if (expected == INVALID_TYPE_ID) {
            out.score_any++;
            out.score_total += 1;
            continue;
        }
        TypeId actual = arg_types[j];
        if (actual == INVALID_TYPE_ID) {
            out.failure_reason = DISPATCH_MATCH_FAIL_TYPE;
            out.failure_arg_index = j;
            return out;
        }
        if (actual == expected) {
            out.score_exact++;
            out.score_total += 100;
        } else if (dispatch_numeric_widening_match(actual, expected, interp)) {
            // Prefer exact Int methods over widened Double methods, but keep widening above generic subtype/Any.
            out.score_widen++;
            out.score_total += 50;
        } else if (interp.types.is_subtype(actual, expected)) {
            out.score_subtype++;
            out.score_total += 10;
        } else {
            out.failure_reason = DISPATCH_MATCH_FAIL_TYPE;
            out.failure_arg_index = j;
            return out;
        }
    }

    if (!method_constraints_satisfied(entry, arg_types, interp)) {
        out.failure_reason = DISPATCH_MATCH_FAIL_CONSTRAINT;
        return out;
    }

    out.matched = true;
    return out;
}

fn SymbolId dispatch_failure_reason_symbol(DispatchMatchFailureReason reason, Interp* interp) @inline {
    switch (reason) {
        case DISPATCH_MATCH_FAIL_ARITY:
            return interp.symbols.intern("arity");
        case DISPATCH_MATCH_FAIL_VALUE_LITERAL:
            return interp.symbols.intern("value-literal");
        case DISPATCH_MATCH_FAIL_TYPE:
            return interp.symbols.intern("type");
        case DISPATCH_MATCH_FAIL_CONSTRAINT:
            return interp.symbols.intern("constraint");
        default:
            return interp.symbols.intern("none");
    }
}

fn Value* dispatch_literal_to_value(ValueLiteralKey* literal, Interp* interp) {
    if (literal == null) return make_nil(interp);
    switch (literal.tag) {
        case NIL:
            if ((uint)literal.sym_value == (uint)interp.sym_false) {
                return make_symbol(interp, interp.sym_false);
            }
            return make_nil(interp);
        case INT:
            return make_int(interp, literal.int_value);
        case SYMBOL:
            return make_symbol(interp, literal.sym_value);
        case STRING:
            return make_string(interp, interp.symbols.get_name(literal.sym_value));
        default:
            return make_nil(interp);
    }
}

fn Value* lambda_expected_value(MethodSignature* sig, usz index, Interp* interp) {
    if (sig == null || index >= sig.param_count) return make_nil(interp);
    if (sig.has_val_literal[index]) {
        return dispatch_literal_to_value(&sig.val_literals[index], interp);
    }

    TypeId expected = sig.param_types[index];
    if (expected != INVALID_TYPE_ID) {
        TypeInfo* ti = interp.types.get(expected);
        if (ti != null) {
            return make_symbol(interp, ti.name);
        }
    }

    SymbolId type_var = sig.param_type_vars[index];
    if ((uint)type_var != 0) {
        return make_symbol(interp, type_var);
    }

    return make_symbol(interp, interp.sym_Any);
}

fn Value* lambda_call_type_error(
    Closure* closure,
    MethodSignature* sig,
    Value*[] args,
    usz provided_arg_count,
    TypeId[] arg_types,
    DispatchMatchBreakdown* breakdown,
    Interp* interp
) {
    Value* data = make_hashmap(interp, 8);
    if (data != null && data.tag == HASHMAP) {
        SymbolId key_failure = interp.symbols.intern("failure");
        SymbolId key_param_index = interp.symbols.intern("param-index");
        SymbolId key_expected = interp.symbols.intern("expected");
        SymbolId key_actual = interp.symbols.intern("actual");
        SymbolId key_expected_arity = interp.symbols.intern("expected-arity");
        SymbolId key_actual_arity = interp.symbols.intern("actual-arity");

        hashmap_set(data.hashmap_val, make_symbol(interp, key_failure),
            make_symbol(interp, dispatch_failure_reason_symbol(breakdown.failure_reason, interp)), interp);
        hashmap_set(data.hashmap_val, make_symbol(interp, key_expected_arity),
            make_int(interp, (long)sig.param_count), interp);
        hashmap_set(data.hashmap_val, make_symbol(interp, key_actual_arity),
            make_int(interp, (long)provided_arg_count), interp);

        if (breakdown.failure_arg_index != usz.max && breakdown.failure_arg_index < args.len) {
            usz idx = breakdown.failure_arg_index;
            hashmap_set(data.hashmap_val, make_symbol(interp, key_param_index), make_int(interp, (long)idx), interp);
            hashmap_set(data.hashmap_val, make_symbol(interp, key_expected), lambda_expected_value(sig, idx, interp), interp);

            Value* actual = make_nil(interp);
            if (idx < arg_types.len) {
                TypeId actual_tid = arg_types[idx];
                if (actual_tid != INVALID_TYPE_ID) {
                    TypeInfo* actual_ti = interp.types.get(actual_tid);
                    if (actual_ti != null) {
                        actual = make_symbol(interp, actual_ti.name);
                    }
                }
            }
            if (actual == null || actual.tag == NIL) {
                actual = make_symbol(interp, value_type_name(args[idx], interp));
            }
            hashmap_set(data.hashmap_val, make_symbol(interp, key_actual), actual, interp);
        } else {
            hashmap_set(data.hashmap_val, make_symbol(interp, key_param_index), make_nil(interp), interp);
            hashmap_set(data.hashmap_val, make_symbol(interp, key_expected), make_nil(interp), interp);
            hashmap_set(data.hashmap_val, make_symbol(interp, key_actual), make_nil(interp), interp);
        }
    }

    char[256] ebuf;
    char[] label = "lambda";
    if (closure != null && (uint)closure.name != 0) {
        label = interp.symbols.get_name(closure.name);
    }

    char[] code = "type/arg-mismatch";
    char[] msg;
    switch (breakdown.failure_reason) {
        case DISPATCH_MATCH_FAIL_ARITY:
            code = "type/arity";
            msg = io::bprintf(&ebuf, "%s: expected %d argument(s), got %d",
                (ZString)label, (int)sig.param_count, (int)provided_arg_count)!!;
        case DISPATCH_MATCH_FAIL_CONSTRAINT:
            msg = io::bprintf(&ebuf, "%s: argument constraints not satisfied",
                (ZString)label)!!;
        case DISPATCH_MATCH_FAIL_VALUE_LITERAL:
            msg = io::bprintf(&ebuf, "%s: literal mismatch at argument %d",
                (ZString)label, (int)breakdown.failure_arg_index)!!;
        case DISPATCH_MATCH_FAIL_TYPE:
            msg = io::bprintf(&ebuf, "%s: type mismatch at argument %d",
                (ZString)label, (int)breakdown.failure_arg_index)!!;
        default:
            msg = io::bprintf(&ebuf, "%s: argument type mismatch", (ZString)label)!!;
    }

    return raise_error_with_payload_names(interp, "type", code, msg, data);
}

fn Value* check_lambda_call_boundary(Closure* closure, Value*[] args, usz provided_arg_count, Interp* interp) {
    if (closure == null || !closure.has_typed_params || closure.type_sig == null) {
        return null;
    }

    MethodSignature* sig = closure.type_sig;
    if (sig.param_count == 0) return null;

    DispatchMatchBreakdown breakdown = {};
    breakdown.failure_reason = DISPATCH_MATCH_FAIL_NONE;
    breakdown.failure_arg_index = usz.max;

    if (args.len != sig.param_count) {
        breakdown.failure_reason = DISPATCH_MATCH_FAIL_ARITY;
        return lambda_call_type_error(closure, sig, args, provided_arg_count, {}, &breakdown, interp);
    }

    TypeId[] arg_types = {};
    TypeId[DISPATCH_INLINE_ARG_CAPACITY] arg_types_inline;
    TypeId* arg_types_storage = null;
    if (args.len > 0) {
        if (args.len <= DISPATCH_INLINE_ARG_CAPACITY) {
            arg_types = arg_types_inline[:args.len];
        } else {
            arg_types_storage = (TypeId*)mem::malloc(TypeId.sizeof * args.len);
            if (arg_types_storage == null) {
                return raise_error(interp, "lambda type check: out of memory");
            }
            arg_types = arg_types_storage[:args.len];
        }
        for (usz i = 0; i < args.len; i++) {
            arg_types[i] = infer_value_type(args[i], interp);
        }
    }
    defer if (arg_types_storage != null) mem::free(arg_types_storage);

    MethodEntry entry = {};
    entry.sig = *sig;
    breakdown = dispatch_match_breakdown(&entry, args, arg_types, interp);
    if (breakdown.matched) return null;

    return lambda_call_type_error(closure, sig, args, provided_arg_count, arg_types, &breakdown, interp);
}

fn Value* find_best_method(MethodTable* mt, Value*[] args, Interp* interp) {
    long best_score = -1;
    Value* best_method = null;
    usz best_count = 0;

    TypeId[] arg_types = {};
    TypeId[DISPATCH_INLINE_ARG_CAPACITY] arg_types_inline;
    TypeId* arg_types_storage = null;
    if (args.len > 0) {
        if (args.len <= DISPATCH_INLINE_ARG_CAPACITY) {
            arg_types = arg_types_inline[:args.len];
        } else {
            arg_types_storage = (TypeId*)mem::malloc(TypeId.sizeof * args.len);
            if (arg_types_storage == null) {
                return raise_error(interp, "method dispatch: out of memory");
            }
            arg_types = arg_types_storage[:args.len];
        }
    }
    defer if (arg_types_storage != null) mem::free(arg_types_storage);

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
