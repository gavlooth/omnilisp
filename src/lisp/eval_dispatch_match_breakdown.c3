module lisp;

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
