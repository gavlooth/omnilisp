module lisp;

import std::io;

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

fn Value* dispatch_reverse_list(Value* list, Interp* interp) {
    Value* out = make_nil(interp);
    Value* cur = list;
    while (cur != null && cur.tag == CONS) {
        out = make_cons(interp, cur.cons_val.car, out);
        cur = cur.cons_val.cdr;
    }
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
        return raise_error(interp, "ambiguous method call: multiple methods match with equal specificity");
    }

    Value* data = make_hashmap(interp, 16);
    SymbolId key_reason = interp.symbols.intern("reason");
    SymbolId key_method = interp.symbols.intern("method");
    SymbolId key_arg_count = interp.symbols.intern("arg-count");
    SymbolId key_arg_types = interp.symbols.intern("arg-types");
    SymbolId key_best_score = interp.symbols.intern("best-score");
    SymbolId key_tie_count = interp.symbols.intern("tie-count");
    SymbolId key_candidate_indices = interp.symbols.intern("candidate-indices");

    Value* arg_types_rev = make_nil(interp);
    for (usz i = 0; i < args.len; i++) {
        SymbolId arg_type_name = value_type_name(args[i], interp);
        arg_types_rev = make_cons(interp, make_symbol(interp, arg_type_name), arg_types_rev);
    }
    Value* arg_types = dispatch_reverse_list(arg_types_rev, interp);

    hashmap_set(data.hashmap_val,
        make_symbol(interp, key_reason),
        make_symbol(interp, interp.symbols.intern("ambiguous-equal-specificity")),
        interp);
    hashmap_set(data.hashmap_val,
        make_symbol(interp, key_method),
        make_symbol(interp, mt.name),
        interp);
    hashmap_set(data.hashmap_val,
        make_symbol(interp, key_arg_count),
        make_int(interp, (long)args.len),
        interp);
    hashmap_set(data.hashmap_val,
        make_symbol(interp, key_arg_types),
        arg_types,
        interp);
    hashmap_set(data.hashmap_val,
        make_symbol(interp, key_best_score),
        make_int(interp, best_score),
        interp);
    hashmap_set(data.hashmap_val,
        make_symbol(interp, key_tie_count),
        make_int(interp, (long)tie_count),
        interp);
    hashmap_set(data.hashmap_val,
        make_symbol(interp, key_candidate_indices),
        dispatch_reverse_list(tie_indices, interp),
        interp);

    return raise_error_with_payload_names(
        interp,
        "type",
        "type/dispatch-ambiguous",
        "ambiguous method call: multiple methods match with equal specificity",
        data
    );
}

/**
 * Format a detailed dispatch error showing method name, arg types, and arity hints.
 */
fn Value* format_dispatch_error(MethodTable* mt, Value*[] args, Interp* interp) {
    char[] name = interp.symbols.get_name(mt.name);

    // Build "Integer, String" arg type list
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
