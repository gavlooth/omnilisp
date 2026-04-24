module lisp;

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
        char[20] digits;
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
            Value* bound_value = null;
            if (template_lookup_binding(bindings, tmpl.sym_val, &bound_value)) return bound_value;

            char[] sym_name = interp.symbols.get_name(tmpl.sym_val);
            if (sym_name.len > 0 && sym_name[sym_name.len - 1] == '#') {
                SymbolId gsym = lookup_or_create_gensym(tmpl.sym_val, gensyms, interp);
                if (gsym == INVALID_SYMBOL_ID) {
                    return make_error(interp, "macro expansion failed to allocate gensym");
                }
                return make_symbol(interp, gsym);
            }

            if (symbol_is_lambda_spelling(&interp.symbols, tmpl.sym_val, interp.sym_lambda)) {
                return make_symbol(interp, interp.sym_lambda);
            }

            Value* captured_value = null;
            if (template_lookup_captured(captured, captured_count, tmpl.sym_val, &captured_value)) {
                return captured_value;
            }
            return tmpl;
        }
        case CONS:
            return expand_template_list(tmpl, bindings, interp, gensyms, captured, captured_count);
        default:
            return tmpl;
    }
}

fn Value* expand_template_splice_target(
    Value* var_sym,
    Value* remaining,
    MatchResult* bindings,
    Interp* interp,
    GensymTable* gensyms,
    CapturedBinding* captured,
    usz captured_count
) {
    if (var_sym == null || var_sym.tag != SYMBOL) {
        return make_error(interp, "macro template splice target must be a symbol");
    }

    Value* spliced = null;
    bool has_binding = template_lookup_binding(bindings, var_sym.sym_val, &spliced);
    if (!has_binding) {
        return make_error(interp, "macro template splice target has no binding");
    }
    if (!is_nil(spliced) && !is_cons(spliced)) {
        return make_error(interp, "macro template splice target must be a list");
    }

    Value* expanded_rest = expand_template_list(remaining, bindings, interp, gensyms, captured, captured_count);
    if (expanded_rest != null && expanded_rest.tag == ERROR) return expanded_rest;
    return append_values(spliced, expanded_rest, interp);
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
        if (!is_cons(rest_after_dotdot)) {
            return make_error(interp, "macro template splice requires a following symbol");
        }
        Value* var_sym = rest_after_dotdot.cons_val.car;
        Value* remaining = rest_after_dotdot.cons_val.cdr;
        return expand_template_splice_target(
            var_sym,
            remaining,
            bindings,
            interp,
            gensyms,
            captured,
            captured_count
        );
    }

    if (head != null && head.tag == CONS && head.cons_val.car != null &&
        head.cons_val.car.tag == SYMBOL && head.cons_val.car.sym_val == interp.sym_dotdot) {
        Value* rest_after_dotdot = head.cons_val.cdr;
        if (!is_cons(rest_after_dotdot)) {
            return make_error(interp, "macro template splice requires a following symbol");
        }
        Value* var_sym = rest_after_dotdot.cons_val.car;
        Value* after_var = rest_after_dotdot.cons_val.cdr;
        if (!is_nil(after_var)) {
            return make_error(interp, "macro template splice expects exactly one symbol argument");
        }
        return expand_template_splice_target(
            var_sym,
            list.cons_val.cdr,
            bindings,
            interp,
            gensyms,
            captured,
            captured_count
        );
    }

    Value* expanded_car = expand_template(head, bindings, interp, gensyms, captured, captured_count);
    if (expanded_car != null && expanded_car.tag == ERROR) return expanded_car;
    Value* expanded_cdr = expand_template_list(list.cons_val.cdr, bindings, interp, gensyms, captured, captured_count);
    if (expanded_cdr != null && expanded_cdr.tag == ERROR) return expanded_cdr;
    return make_cons(interp, expanded_car, expanded_cdr);
}

/**
 * Append two value lists.
 */
fn Value* append_values(Value* a, Value* b, Interp* interp, usz depth = 0) {
    if (a != null && a.tag == ERROR) return a;
    if (b != null && b.tag == ERROR) return b;
    if (is_nil(a)) return b;
    if (!is_cons(a)) return make_error(interp, "macro template splice target must be a proper list");
    if (depth >= 10000) return make_error(interp, "macro template splice exceeded recursion limit");
    return make_cons(interp, a.cons_val.car, append_values(a.cons_val.cdr, b, interp, depth + 1));
}
