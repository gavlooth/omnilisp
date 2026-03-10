module lisp;

/**
 * Expand a pattern-based macro.
 * Converts macro call args to a value list, tries each clause's pattern,
 * and on first match, expands the template with bindings.
 */
fn EvalResult expand_pattern_macro(MacroDef* mdef, Expr* call_expr, Interp* interp) {
    usz arg_count = call_expr.call.arg_count;
    Value* args_list = make_nil(interp);
    for (usz i = arg_count; i > 0; i--) {
        Value* arg_val = expr_to_value(call_expr.call.args[i - 1], interp);
        args_list = make_cons(interp, arg_val, args_list);
    }

    for (usz i = 0; i < mdef.clause_count; i++) {
        MatchResult match_result = match_pattern(mdef.clauses[i].pattern, args_list, interp);
        if (match_result.matched) {
            GensymTable gensyms;
            gensym_table_init(&gensyms);
            defer gensym_table_destroy(&gensyms);
            Value* expanded = expand_template(mdef.clauses[i].tmpl, &match_result, interp, &gensyms,
                                              mdef.captured_bindings, mdef.captured_count);
            match_result.cleanup();
            return eval_ok(expanded);
        }
    }

    return eval_error("no macro clause matched");
}

/**
 * Look up or create a gensym for an auto-gensym symbol (ending with #).
 * Within a single macro expansion, the same #-symbol always maps to the same gensym.
 */
fn SymbolId lookup_or_create_gensym(SymbolId original, GensymTable* table, Interp* interp) {
    for (usz i = 0; i < table.count; i++) {
        if ((uint)table.mappings[i].original == (uint)original) {
            return table.mappings[i].generated;
        }
    }

    char[32] buf;
    usz len = 0;
    buf[0] = 'g';
    buf[1] = '#';
    len = 2;

    interp.gensym_counter++;
    usz counter = interp.gensym_counter;
    if (counter == 0) {
        buf[len++] = '0';
    } else {
        char[16] digits;
        usz dcount = 0;
        usz tmp = counter;
        while (tmp > 0) {
            digits[dcount++] = (char)('0' + (tmp % 10));
            tmp /= 10;
        }
        for (usz i = dcount; i > 0; i--) {
            buf[len++] = digits[i - 1];
        }
    }
    SymbolId gsym = interp.symbols.intern(buf[:len]);
    if (gsym == INVALID_SYMBOL_ID) return INVALID_SYMBOL_ID;

    if (!gensym_table_ensure_capacity(table, table.count + 1)) return INVALID_SYMBOL_ID;
    table.mappings[table.count].original = original;
    table.mappings[table.count].generated = gsym;
    table.count++;
    return gsym;
}

/**
 * Expand a macro template with pattern bindings.
 * Symbols in bindings are substituted; literal symbols stay.
 * ".." before a symbol splices the bound list.
 * Symbols ending with # are auto-replaced with unique gensyms for hygiene.
 *
 * HYGIENE: Template literal symbols (not pattern vars, not gensyms, not special forms)
 * are resolved through captured_bindings — a snapshot of definition-time values.
 * This prevents expansion-site shadowing from capturing macro-internal references.
 */
fn Value* expand_template(Value* tmpl, MatchResult* bindings, Interp* interp, GensymTable* gensyms,
                          CapturedBinding* captured, usz captured_count) {
    if (tmpl == null) return make_nil(interp);

    switch (tmpl.tag) {
        case SYMBOL: {
            for (usz i = 0; i < bindings.binding_count; i++) {
                if ((uint)bindings.bindings[i].name == (uint)tmpl.sym_val) {
                    return bindings.bindings[i].value;
                }
            }

            char[] sym_name = interp.symbols.get_name(tmpl.sym_val);
            if (sym_name.len > 0 && sym_name[sym_name.len - 1] == '#') {
                SymbolId gsym = lookup_or_create_gensym(tmpl.sym_val, gensyms, interp);
                return make_symbol(interp, gsym);
            }

            for (usz i = 0; i < captured_count; i++) {
                if ((uint)captured[i].sym == (uint)tmpl.sym_val) {
                    return captured[i].value;
                }
            }
            return tmpl;
        }
        case CONS:
            return expand_template_list(tmpl, bindings, interp, gensyms, captured, captured_count);
        default:
            return tmpl;
    }
}

/**
 * Expand a template list, handling ".." splice.
 * When we see (.. var rest...) in a list, we splice var's bound value.
 */
fn Value* expand_template_list(Value* list, MatchResult* bindings, Interp* interp, GensymTable* gensyms,
                               CapturedBinding* captured, usz captured_count) {
    if (is_nil(list)) return make_nil(interp);
    if (!is_cons(list)) return expand_template(list, bindings, interp, gensyms, captured, captured_count);

    Value* head = list.cons_val.car;

    if (head != null && head.tag == SYMBOL && head.sym_val == interp.sym_dotdot) {
        Value* rest_after_dotdot = list.cons_val.cdr;
        if (is_cons(rest_after_dotdot)) {
            Value* var_sym = rest_after_dotdot.cons_val.car;
            Value* remaining = rest_after_dotdot.cons_val.cdr;

            Value* spliced = make_nil(interp);
            if (var_sym != null && var_sym.tag == SYMBOL) {
                for (usz i = 0; i < bindings.binding_count; i++) {
                    if ((uint)bindings.bindings[i].name == (uint)var_sym.sym_val) {
                        spliced = bindings.bindings[i].value;
                        break;
                    }
                }
            }

            Value* expanded_rest = expand_template_list(remaining, bindings, interp, gensyms, captured, captured_count);
            return append_values(spliced, expanded_rest, interp);
        }
    }

    Value* expanded_car = expand_template(head, bindings, interp, gensyms, captured, captured_count);
    Value* expanded_cdr = expand_template_list(list.cons_val.cdr, bindings, interp, gensyms, captured, captured_count);
    return make_cons(interp, expanded_car, expanded_cdr);
}

/**
 * Append two value lists.
 */
fn Value* append_values(Value* a, Value* b, Interp* interp, usz depth = 0) {
    if (is_nil(a)) return b;
    if (!is_cons(a)) return b;
    if (depth >= 10000) return make_nil(interp);
    return make_cons(interp, a.cons_val.car, append_values(a.cons_val.cdr, b, interp, depth + 1));
}
