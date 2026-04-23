module lisp;

import std::io;

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
        if (try char[] formatted = io::bprintf(&buf, "no matching method '%s': got %d arg(s) (%s), expected %s arg(s)",
            (ZString)name, (int)args.len, arg_types, arities.zstr_view())) {
            msg = formatted;
        } else {
            msg = "no matching method: arity mismatch";
        }
    } else {
        if (try char[] formatted = io::bprintf(&buf, "no matching method '%s' for (%s)",
            (ZString)name, arg_types)) {
            msg = formatted;
        } else {
            msg = "no matching method";
        }
    }
    return runtime_raise(interp, "runtime/evaluation-error", msg);
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
                    char[] msg;
                    if (try char[] formatted = io::bprintf(&buf, "no pattern matched: %s value '%s', missing variants: %s",
                        (ZString)parent_name, (ZString)type_name, missing.zstr_view())) {
                        msg = formatted;
                    } else {
                        msg = "no pattern matched: missing union variants";
                    }
                    return runtime_raise(interp, "runtime/evaluation-error", msg);
                }
            }
        }
    }

    // Fallback for non-union types
    char[] msg;
    if (try char[] formatted = io::bprintf(&buf, "no pattern matched for value of type %s", (ZString)type_name)) {
        msg = formatted;
    } else {
        msg = "no pattern matched";
    }
    return runtime_raise(interp, "runtime/evaluation-error", msg);
}
