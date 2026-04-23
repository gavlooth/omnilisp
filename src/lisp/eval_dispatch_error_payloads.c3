module lisp;

import std::io;

fn Value* dispatch_make_payload_data(Interp* interp, uint capacity) @inline {
    Value* data = make_hashmap_no_raise(interp, capacity);
    return data != null && data.tag == HASHMAP ? data : null;
}

fn bool dispatch_payload_set(Value* data, SymbolId key, Value* value, Interp* interp) @inline {
    if (key == INVALID_SYMBOL_ID || value == null) return false;
    if (value.tag == ERROR || (value.tag == SYMBOL && value.sym_val == INVALID_SYMBOL_ID)) return false;
    return data != null &&
        data.tag == HASHMAP &&
        hashmap_set_checked(data.hashmap_val, make_symbol(interp, key), value, interp);
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
    Value* data = dispatch_make_payload_data(interp, 8);
    if (data != null) {
        SymbolId key_failure = interp.symbols.intern("failure");
        SymbolId key_param_index = interp.symbols.intern("param-index");
        SymbolId key_expected = interp.symbols.intern("expected");
        SymbolId key_actual = interp.symbols.intern("actual");
        SymbolId key_expected_arity = interp.symbols.intern("expected-arity");
        SymbolId key_actual_arity = interp.symbols.intern("actual-arity");

        bool payload_ok =
            dispatch_payload_set(data, key_failure,
                make_symbol(interp, dispatch_failure_reason_symbol(breakdown.failure_reason, interp)), interp) &&
            dispatch_payload_set(data, key_expected_arity, make_int(interp, (long)sig.param_count), interp) &&
            dispatch_payload_set(data, key_actual_arity, make_int(interp, (long)provided_arg_count), interp);

        if (payload_ok && breakdown.failure_arg_index != usz.max && breakdown.failure_arg_index < args.len) {
            usz idx = breakdown.failure_arg_index;
            payload_ok =
                dispatch_payload_set(data, key_param_index, make_int(interp, (long)idx), interp) &&
                dispatch_payload_set(data, key_expected, lambda_expected_value(sig, idx, interp), interp);

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
            payload_ok = payload_ok && dispatch_payload_set(data, key_actual, actual, interp);
        } else if (payload_ok) {
            payload_ok =
                dispatch_payload_set(data, key_param_index, make_nil(interp), interp) &&
                dispatch_payload_set(data, key_expected, make_nil(interp), interp) &&
                dispatch_payload_set(data, key_actual, make_nil(interp), interp);
        }

        if (!payload_ok) {
            data = null;
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
            if (try char[] formatted = io::bprintf(&ebuf, "%s: expected %d argument(s), got %d",
                (ZString)label, (int)sig.param_count, (int)provided_arg_count)) {
                msg = formatted;
            } else {
                msg = "lambda: arity mismatch";
            }
            break;
        case DISPATCH_MATCH_FAIL_CONSTRAINT:
            if (try char[] formatted = io::bprintf(&ebuf, "%s: argument constraints not satisfied",
                (ZString)label)) {
                msg = formatted;
            } else {
                msg = "lambda: argument constraints not satisfied";
            }
            break;
        case DISPATCH_MATCH_FAIL_VALUE_LITERAL:
            if (try char[] formatted = io::bprintf(&ebuf, "%s: literal mismatch at argument %d",
                (ZString)label, (int)breakdown.failure_arg_index)) {
                msg = formatted;
            } else {
                msg = "lambda: literal mismatch";
            }
            break;
        case DISPATCH_MATCH_FAIL_TYPE:
            if (try char[] formatted = io::bprintf(&ebuf, "%s: type mismatch at argument %d",
                (ZString)label, (int)breakdown.failure_arg_index)) {
                msg = formatted;
            } else {
                msg = "lambda: argument type mismatch";
            }
            break;
        default:
            if (try char[] formatted = io::bprintf(&ebuf, "%s: argument type mismatch", (ZString)label)) {
                msg = formatted;
            } else {
                msg = "lambda: argument type mismatch";
            }
            break;
    }

    return raise_error_with_payload_names(interp, "type", code, msg, data);
}

fn Value* dispatch_reverse_list(Value* list, Interp* interp) {
    Value* out = make_nil(interp);
    Value* cur = list;
    while (cur != null && cur.tag == CONS) {
        out = make_cons(interp, cur.cons_val.car, out);
        cur = cur.cons_val.cdr;
    }
    if (!is_nil(cur)) return runtime_raise(interp, "type/arg-mismatch", "__reverse-list: expected a proper list");
    return out;
}

fn Value* dispatch_ambiguous_error(
    MethodTable* mt,
    Value*[] args,
    long best_score,
    usz tie_count,
    Value* tie_indices,
    Interp* interp
) {
    if (mt == null || interp == null) {
        return runtime_raise(interp, "runtime/evaluation-error", "ambiguous method call: multiple methods match with equal specificity");
    }

    Value* data = dispatch_make_payload_data(interp, 16);
    SymbolId key_reason = interp.symbols.intern("reason");
    SymbolId key_method = interp.symbols.intern("method");
    SymbolId key_arg_count = interp.symbols.intern("arg-count");
    SymbolId key_arg_types = interp.symbols.intern("arg-types");
    SymbolId key_best_score = interp.symbols.intern("best-score");
    SymbolId key_tie_count = interp.symbols.intern("tie-count");
    SymbolId key_candidate_indices = interp.symbols.intern("candidate-indices");

    Value* arg_types_rev = make_nil(interp);
foreach (arg : args) {
        SymbolId arg_type_name = value_type_name(arg, interp);
        arg_types_rev = make_cons(interp, make_symbol(interp, arg_type_name), arg_types_rev);
}
    Value* arg_types = dispatch_reverse_list(arg_types_rev, interp);

    if (data != null) {
        SymbolId reason_ambiguous = interp.symbols.intern("ambiguous-equal-specificity");
        bool payload_ok =
            dispatch_payload_set(data, key_reason,
                make_symbol(interp, reason_ambiguous), interp) &&
            dispatch_payload_set(data, key_method, make_symbol(interp, mt.name), interp) &&
            dispatch_payload_set(data, key_arg_count, make_int(interp, (long)args.len), interp) &&
            dispatch_payload_set(data, key_arg_types, arg_types, interp) &&
            dispatch_payload_set(data, key_best_score, make_int(interp, best_score), interp) &&
            dispatch_payload_set(data, key_tie_count, make_int(interp, (long)tie_count), interp) &&
            dispatch_payload_set(data, key_candidate_indices, dispatch_reverse_list(tie_indices, interp), interp);
        if (!payload_ok) {
            data = null;
        }
    }

    return raise_error_with_payload_names(
        interp,
        "type",
        "type/dispatch-ambiguous",
        "ambiguous method call: multiple methods match with equal specificity",
        data
    );
}
