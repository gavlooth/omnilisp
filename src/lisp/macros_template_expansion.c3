module lisp;

fn bool template_lookup_binding(MatchResult* bindings, SymbolId name, Value** out_value) {
    for (usz i = 0; i < bindings.binding_count; i++) {
        if ((uint)bindings.bindings[i].name == (uint)name) {
            *out_value = bindings.bindings[i].value;
            return true;
        }
    }
    return false;
}

fn bool template_lookup_captured(CapturedBinding* captured, usz captured_count, SymbolId name, Value** out_value) {
    for (usz i = 0; i < captured_count; i++) {
        if ((uint)captured[i].sym == (uint)name) {
            *out_value = captured[i].value;
            return true;
        }
    }
    return false;
}

fn bool macro_is_proper_list(Value* v) @inline {
    while (is_cons(v)) v = v.cons_val.cdr;
    return is_nil(v);
}

fn usz macro_list_length(Value* v) @inline {
    usz n = 0;
    while (is_cons(v)) {
        n++;
        v = v.cons_val.cdr;
    }
    return n;
}

fn bool macro_list_has_exact_arity(Value* args, usz expected) @inline {
    if (!macro_is_proper_list(args)) return false;
    return macro_list_length(args) == expected;
}

fn bool macro_list_has_min_arity(Value* args, usz min_expected) @inline {
    if (!macro_is_proper_list(args)) return false;
    return macro_list_length(args) >= min_expected;
}

fn char[] macro_validate_pattern(Pattern* pat) {
    if (pat == null) return "bad macro pattern: null pattern";

    switch (pat.tag) {
        case PAT_WILDCARD:
        case PAT_VAR:
        case PAT_LIT:
        case PAT_QUOTE:
            return "";

        case PAT_SEQ:
            if (pat.elem_count > 0 && pat.elements == null) {
                return "bad macro pattern: sequence pattern elements missing";
            }
            if (pat.rest_pos == REST_MIDDLE && (uint)pat.rest_binding == 0) {
                return "bad macro pattern: rest binding name is missing";
            }
            if ((pat.rest_pos == REST_START || pat.rest_pos == REST_END) && (uint)pat.rest_binding != 0) {
                return "bad macro pattern: unexpected rest binding name";
            }
            for (usz i = 0; i < pat.elem_count; i++) {
                char[] sub_err = macro_validate_pattern(pat.elements[i]);
                if (sub_err.len > 0) return sub_err;
            }
            return "";

        case PAT_CONS:
            return "bad macro pattern: cons-cell pattern is not supported in syntax-match";
        case PAT_AS:
            if ((uint)pat.as_name == 0) return "bad macro pattern: as-pattern binding name is missing";
            return macro_validate_pattern(pat.as_sub);
        case PAT_CONSTRUCTOR:
            return "bad macro pattern: constructor pattern is not supported in syntax-match";
        case PAT_GUARD:
            return "bad macro pattern: guard pattern is not supported in syntax-match";
        case PAT_DICT:
            return "bad macro pattern: dict pattern is not supported in syntax-match";
        default:
            return "bad macro pattern: unknown pattern tag";
    }
}

fn bool macro_validate_lambda_params(Value* params) @inline {
    if (!macro_is_proper_list(params)) return false;
    while (is_cons(params)) {
        Value* p = params.cons_val.car;
        if (p == null || p.tag != SYMBOL) return false;
        params = params.cons_val.cdr;
    }
    return true;
}

fn char[] macro_validate_emitted_form(Value* val, Interp* interp, usz depth = 0) {
    if (val == null || is_nil(val)) return "";
    if (val.tag == ERROR) return val.str_chars[:val.str_len];
    if (val.tag != CONS) return "";
    if (depth >= 4096) return "malformed emitted form: expansion nesting is too deep";
    if (!macro_is_proper_list(val)) return "malformed emitted form: expected a proper list";

    Value* head = val.cons_val.car;
    Value* rest = val.cons_val.cdr;

    if (head != null && head.tag == SYMBOL) {
        SymbolId sym = head.sym_val;
        if ((uint)sym == (uint)interp.sym_quote) {
            if (!macro_list_has_exact_arity(rest, 1)) return "malformed emitted form: quote expects exactly 1 argument";
            return "";
        }

        if ((uint)sym == (uint)interp.sym_if) {
            if (!macro_list_has_exact_arity(rest, 3)) return "malformed emitted form: if expects exactly 3 arguments";
        } else if ((uint)sym == (uint)interp.sym_lambda) {
            if (!macro_list_has_exact_arity(rest, 2)) return "malformed emitted form: lambda expects parameter list and body";
            Value* params = rest.cons_val.car;
            if (!macro_validate_lambda_params(params)) {
                return "malformed emitted form: lambda parameters must be a proper symbol list";
            }
        } else if ((uint)sym == (uint)interp.sym_let) {
            if (!macro_list_has_exact_arity(rest, 2)) return "malformed emitted form: let expects bindings and body";
        } else if ((uint)sym == (uint)interp.sym_define) {
            if (!macro_list_has_exact_arity(rest, 2)) return "malformed emitted form: define expects name and value";
            Value* name_v = rest.cons_val.car;
            if (name_v == null || name_v.tag != SYMBOL) return "malformed emitted form: define name must be a symbol";
        } else if ((uint)sym == (uint)interp.sym_set) {
            if (!macro_list_has_exact_arity(rest, 2)) return "malformed emitted form: set! expects name and value";
            Value* name_v = rest.cons_val.car;
            if (name_v == null || name_v.tag != SYMBOL) return "malformed emitted form: set! name must be a symbol";
        } else if ((uint)sym == (uint)interp.sym_and || (uint)sym == (uint)interp.sym_or) {
            if (!macro_list_has_min_arity(rest, 0)) return "malformed emitted form: and/or arguments must be a proper list";
        } else if ((uint)sym == (uint)interp.sym_checkpoint) {
            if (!macro_list_has_exact_arity(rest, 1)) return "malformed emitted form: checkpoint expects exactly 1 argument";
        } else if ((uint)sym == (uint)interp.sym_capture) {
            if (!macro_list_has_exact_arity(rest, 2)) return "malformed emitted form: capture expects continuation name and body";
            Value* k_name = rest.cons_val.car;
            if (k_name == null || k_name.tag != SYMBOL) return "malformed emitted form: capture continuation name must be a symbol";
        } else if ((uint)sym == (uint)interp.sym_quasiquote ||
                   (uint)sym == (uint)interp.sym_unquote ||
                   (uint)sym == (uint)interp.sym_unquote_splicing) {
            if (!macro_list_has_exact_arity(rest, 1)) {
                return "malformed emitted form: quasiquote/unquote expects exactly 1 argument";
            }
        } else if ((uint)sym == (uint)interp.sym_signal) {
            if (!macro_list_has_exact_arity(rest, 2)) return "malformed emitted form: signal expects tag and payload";
            Value* tag_v = rest.cons_val.car;
            if (tag_v == null || tag_v.tag != SYMBOL) return "malformed emitted form: signal tag must be a symbol";
        } else if ((uint)sym == (uint)interp.sym_block) {
            if (!macro_list_has_min_arity(rest, 0)) return "malformed emitted form: block arguments must be a proper list";
        }
    }

    Value* cursor = val;
    while (is_cons(cursor)) {
        char[] sub_err = macro_validate_emitted_form(cursor.cons_val.car, interp, depth + 1);
        if (sub_err.len > 0) return sub_err;
        cursor = cursor.cons_val.cdr;
    }
    return "";
}

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
        char[] pattern_err = macro_validate_pattern(mdef.clauses[i].pattern);
        if (pattern_err.len > 0) return eval_error(pattern_err);

        MatchResult match_result = match_pattern(mdef.clauses[i].pattern, args_list, interp);
        if (match_result.has_error()) {
            char[] msg = match_result.error != null && match_result.error.tag == ERROR
                ? match_result.error.str_chars[:match_result.error.str_len]
                : "pattern match failed";
            match_result.cleanup();
            return eval_error(msg);
        }
        if (match_result.matched) {
            GensymTable gensyms;
            gensym_table_init(&gensyms);
            defer gensym_table_destroy(&gensyms);
            Value* expanded = expand_template(mdef.clauses[i].tmpl, &match_result, interp, &gensyms,
                                              mdef.captured_bindings, mdef.captured_count);
            if (expanded != null && expanded.tag == ERROR) {
                match_result.cleanup();
                return eval_error(expanded.str_chars[:expanded.str_len]);
            }
            char[] emitted_err = macro_validate_emitted_form(expanded, interp);
            if (emitted_err.len > 0) {
                match_result.cleanup();
                return eval_error(emitted_err);
            }
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
            Value* bound_value = null;
            if (template_lookup_binding(bindings, tmpl.sym_val, &bound_value)) return bound_value;

            char[] sym_name = interp.symbols.get_name(tmpl.sym_val);
            if (sym_name.len > 0 && sym_name[sym_name.len - 1] == '#') {
                SymbolId gsym = lookup_or_create_gensym(tmpl.sym_val, gensyms, interp);
                return make_symbol(interp, gsym);
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
    if (!is_cons(a)) return b;
    if (depth >= 10000) return make_nil(interp);
    return make_cons(interp, a.cons_val.car, append_values(a.cons_val.cdr, b, interp, depth + 1));
}
