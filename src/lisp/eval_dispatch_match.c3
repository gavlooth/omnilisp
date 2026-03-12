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
    // Numeric promotion policy (dispatch-only): Integer can satisfy Double-typed methods.
    return actual == interp.tid_Integer && expected == interp.tid_Double;
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
        TypeId actual_canonical = interp.types.resolve_alias(actual);
        TypeId expected_canonical = interp.types.resolve_alias(expected);
        if (actual_canonical == expected_canonical) {
            out.score_exact++;
            out.score_total += 100;
        } else if (dispatch_numeric_widening_match(actual_canonical, expected_canonical, interp)) {
            // Prefer exact Integer methods over widened Double methods, but keep widening above generic subtype/Any.
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
