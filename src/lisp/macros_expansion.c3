module lisp;

import std::collections::list;
import main;
// =============================================================================
// SECTION 2.6/2.7: macro expansion + module helpers
// =============================================================================

fn MacroDef* lookup_macro(SymbolId name, Interp* interp) {
    usz hash_slot = (usz)name % interp.macro_hash_capacity;
    for (usz probe = 0; probe < interp.macro_hash_capacity; probe++) {
        usz idx = interp.macro_hash_index[hash_slot];
        if (idx == usz.max) return null;  // Empty slot — not found
        if ((uint)interp.macro_table[idx].name == (uint)name) {
            return &interp.macro_table[idx];
        }
        hash_slot = (hash_slot + 1) % interp.macro_hash_capacity;
    }
    return null;
}

fn usz lookup_macro_index(SymbolId name, Interp* interp) {
    usz hash_slot = (usz)name % interp.macro_hash_capacity;
    for (usz probe = 0; probe < interp.macro_hash_capacity; probe++) {
        usz idx = interp.macro_hash_index[hash_slot];
        if (idx == usz.max) return usz.max;
        if ((uint)interp.macro_table[idx].name == (uint)name) return idx;
        hash_slot = (hash_slot + 1) % interp.macro_hash_capacity;
    }
    return usz.max;
}

fn void macro_def_destroy(MacroDef* mdef) {
    if (mdef == null) return;
    if (mdef.clauses != null) mem::free(mdef.clauses);
    if (mdef.captured_bindings != null) mem::free(mdef.captured_bindings);
    mdef.clauses = null;
    mdef.captured_bindings = null;
    mdef.clause_count = 0;
    mdef.captured_count = 0;
}

fn bool macro_def_copy_clauses(MacroDef* out, MacroClause* src, usz count) {
    if (count == 0) {
        out.clauses = null;
        out.clause_count = 0;
        return true;
    }
    out.clauses = (MacroClause*)mem::malloc(MacroClause.sizeof * count);
    if (out.clauses == null) return false;
    for (usz i = 0; i < count; i++) out.clauses[i] = src[i];
    out.clause_count = count;
    return true;
}

fn bool macro_def_copy_captured_bindings(MacroDef* out, List{CapturedBinding}* bindings) {
    if (bindings.len() == 0) {
        out.captured_bindings = null;
        out.captured_count = 0;
        return true;
    }
    out.captured_bindings = (CapturedBinding*)mem::malloc(CapturedBinding.sizeof * bindings.len());
    if (out.captured_bindings == null) return false;
    for (usz i = 0; i < bindings.len(); i++) out.captured_bindings[i] = (*bindings)[i];
    out.captured_count = bindings.len();
    return true;
}

/**
 * Collect pattern variable names from a Pattern into a flat array.
 * Used by eval_define_macro to distinguish pattern vars from template literals.
 */
fn void collect_pattern_vars(Pattern* pat, List{SymbolId}* vars) {
    if (pat == null) return;
    switch (pat.tag) {
        case PAT_VAR: {
            vars.push(pat.var_name);
        }
        case PAT_CONS: {
            collect_pattern_vars(pat.car_pat, vars);
            collect_pattern_vars(pat.cdr_pat, vars);
        }
        case PAT_SEQ: {
            for (usz i = 0; i < pat.elem_count; i++) {
                collect_pattern_vars(pat.elements[i], vars);
            }
            // Rest binding is also a pattern var
            if (pat.rest_pos != REST_NONE) {
                vars.push(pat.rest_binding);
            }
        }
        case PAT_AS: {
            collect_pattern_vars(pat.as_sub, vars);
            vars.push(pat.as_name);
        }
        case PAT_CONSTRUCTOR: {
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                collect_pattern_vars(pat.ctor_sub_patterns[i], vars);
            }
        }
        default: {}
    }
}

/**
 * Walk a template Value* tree and capture bindings for non-pattern-var,
 * non-gensym, non-special-form symbols from the current global env.
 * This creates a SNAPSHOT of the definition-time bindings.
 */
fn void capture_template_bindings(Value* tmpl, List{SymbolId}* pat_vars,
                                  List{CapturedBinding}* bindings, Interp* interp) {
    if (tmpl == null) return;

    if (tmpl.tag == SYMBOL) {
        SymbolId sym = tmpl.sym_val;

        // Skip pattern variables
        for (usz i = 0; i < pat_vars.len(); i++) {
            if ((uint)(*pat_vars)[i] == (uint)sym) return;
        }

        // Skip auto-gensym symbols (ending with #)
        char[] sym_name = interp.symbols.get_name(sym);
        if (sym_name.len > 0 && sym_name[sym_name.len - 1] == '#') return;

        // Skip special form symbols
        if (is_special_form_symbol(sym, interp)) return;

        // Skip if already captured
        for (usz i = 0; i < bindings.len(); i++) {
            if ((uint)(*bindings)[i].sym == (uint)sym) return;
        }

        // Look up in global env — if defined, capture it
        if (interp.global_env != null) {
            Value* val = interp.global_env.lookup(sym);
            if (val != null) {
                bindings.push({ .sym = sym, .value = val });
            }
        }
        return;
    }

    if (tmpl.tag == CONS) {
        capture_template_bindings(tmpl.cons_val.car, pat_vars, bindings, interp);
        capture_template_bindings(tmpl.cons_val.cdr, pat_vars, bindings, interp);
    }
}

fn void capture_template_bindings_in_root_scope(
    Value* tmpl,
    List{SymbolId}* pat_vars,
    List{CapturedBinding}* bindings,
    Interp* interp
) {
    boundary_assert_interp_scope_chain(interp);
    main::ScopeRegion* saved_scope = boundary_enter_scope(interp, interp.root_scope);
    defer boundary_leave_scope(interp, saved_scope);
    capture_template_bindings(tmpl, pat_vars, bindings, interp);
    boundary_assert_interp_scope_chain(interp);
}

/**
 * Evaluate a define-macro expression.
 * (define [macro] name (pattern1 template1) ...)
 * Captures definition-time bindings for template hygiene.
 */
<* @require expr != null : "eval_define_macro: null expr"
   @require interp != null : "eval_define_macro: null interp" *>
fn EvalResult eval_define_macro(Expr* expr, Env* env, Interp* interp) {
    MacroDef new_def = {};
    new_def.name = expr.define_macro.name;
    if (!macro_def_copy_clauses(&new_def, expr.define_macro.clauses, expr.define_macro.clause_count)) {
        return eval_error("define-macro: out of memory while copying clauses");
    }

    List{CapturedBinding} captures;
    defer captures.free();

    // Capture definition-time bindings for hygiene.
    // For each clause, collect pattern vars, then scan template for literals
    // that have current global-env bindings, and snapshot those values.
    for (usz i = 0; i < expr.define_macro.clause_count; i++) {
        // Collect pattern variable names for this clause
        List{SymbolId} pat_vars;
        defer pat_vars.free();
        collect_pattern_vars(new_def.clauses[i].pattern, &pat_vars);

        // Walk template and capture bindings
        capture_template_bindings_in_root_scope(
            new_def.clauses[i].tmpl,
            &pat_vars,
            &captures,
            interp
        );
    }
    if (!macro_def_copy_captured_bindings(&new_def, &captures)) {
        macro_def_destroy(&new_def);
        return eval_error("define-macro: out of memory while capturing bindings");
    }

    usz macro_idx = lookup_macro_index(new_def.name, interp);
    if (macro_idx != usz.max) {
        macro_def_destroy(&interp.macro_table[macro_idx]);
        interp.macro_table[macro_idx] = new_def;
        return eval_ok(make_nil(interp));
    }

    if (interp.macro_count >= interp.macro_capacity) interp.grow_macro_table();
    macro_idx = interp.macro_count;
    interp.macro_table[macro_idx] = new_def;
    interp.macro_count++;

    // Insert into macro hash index
    usz hash_slot = (usz)new_def.name % interp.macro_hash_capacity;
    for (;;) {
        usz existing = interp.macro_hash_index[hash_slot];
        if (existing == usz.max) {
            // Empty slot — insert here
            interp.macro_hash_index[hash_slot] = macro_idx;
            break;
        }
        if ((uint)interp.macro_table[existing].name == (uint)new_def.name) {
            // Same name (redefinition) — update to point to new entry
            interp.macro_hash_index[hash_slot] = macro_idx;
            break;
        }
        hash_slot = (hash_slot + 1) % interp.macro_hash_capacity;
    }
    return eval_ok(make_nil(interp));
}

fn void expand_macros_in_sequence(Expr** exprs, usz expr_count, Interp* interp) {
    for (usz i = 0; i < expr_count; i++) {
        Expr* child = exprs[i];
        if (child != null && child.tag == E_DEFMACRO) {
            eval_define_macro(child, interp.global_env, interp);
            continue;
        }
        exprs[i] = expand_macros_in_expr(child, interp);
    }
}

fn Expr* expand_macro_call_site(Expr* expr, Interp* interp) {
    if (expr.call.func == null || expr.call.func.tag != E_VAR) return null;
    MacroDef* mdef = lookup_macro(expr.call.func.var_expr.name, interp);
    if (mdef == null) return null;

    EvalResult r = expand_pattern_macro(mdef, expr, interp);
    if (r.error.has_error) return null;

    Expr* expanded = value_to_expr(r.value, interp);
    return expand_macros_in_expr(expanded, interp);
}

fn void expand_macros_call_children(Expr* expr, Interp* interp) {
    expr.call.func = expand_macros_in_expr(expr.call.func, interp);
    for (usz i = 0; i < expr.call.arg_count; i++) {
        expr.call.args[i] = expand_macros_in_expr(expr.call.args[i], interp);
    }
}

fn bool expand_macros_unary(Expr* expr, Interp* interp) {
    switch (expr.tag) {
        case E_DEFINE:
            expr.define.value = expand_macros_in_expr(expr.define.value, interp);
            return true;
        case E_LAMBDA:
            expr.lambda.body = expand_macros_in_expr(expr.lambda.body, interp);
            return true;
        case E_SET:
            expr.set_expr.value = expand_macros_in_expr(expr.set_expr.value, interp);
            return true;
        case E_RESET:
            expr.reset.body = expand_macros_in_expr(expr.reset.body, interp);
            return true;
        case E_SHIFT:
            expr.shift.body = expand_macros_in_expr(expr.shift.body, interp);
            return true;
        case E_PERFORM:
            expr.perform.arg = expand_macros_in_expr(expr.perform.arg, interp);
            return true;
        case E_RESOLVE:
            expr.resolve.value = expand_macros_in_expr(expr.resolve.value, interp);
            return true;
        default:
            return false;
    }
}

fn bool expand_macros_binary(Expr* expr, Interp* interp) {
    switch (expr.tag) {
        case E_IF:
            expr.if_expr.test = expand_macros_in_expr(expr.if_expr.test, interp);
            expr.if_expr.then_branch = expand_macros_in_expr(expr.if_expr.then_branch, interp);
            expr.if_expr.else_branch = expand_macros_in_expr(expr.if_expr.else_branch, interp);
            return true;
        case E_LET:
            expr.let_expr.init = expand_macros_in_expr(expr.let_expr.init, interp);
            expr.let_expr.body = expand_macros_in_expr(expr.let_expr.body, interp);
            return true;
        case E_APP:
            expr.app.func = expand_macros_in_expr(expr.app.func, interp);
            expr.app.arg = expand_macros_in_expr(expr.app.arg, interp);
            return true;
        case E_AND:
            expr.and_expr.left = expand_macros_in_expr(expr.and_expr.left, interp);
            expr.and_expr.right = expand_macros_in_expr(expr.and_expr.right, interp);
            return true;
        case E_OR:
            expr.or_expr.left = expand_macros_in_expr(expr.or_expr.left, interp);
            expr.or_expr.right = expand_macros_in_expr(expr.or_expr.right, interp);
            return true;
        case E_INDEX:
            expr.index.collection = expand_macros_in_expr(expr.index.collection, interp);
            expr.index.index = expand_macros_in_expr(expr.index.index, interp);
            return true;
        default:
            return false;
    }
}

fn bool expand_macros_complex(Expr* expr, Interp* interp) {
    switch (expr.tag) {
        case E_HANDLE:
            expr.handle.body = expand_macros_in_expr(expr.handle.body, interp);
            for (usz i = 0; i < expr.handle.clause_count; i++) {
                expr.handle.clauses[i].handler_body = expand_macros_in_expr(expr.handle.clauses[i].handler_body, interp);
            }
            return true;
        case E_MATCH:
            expr.match.scrutinee = expand_macros_in_expr(expr.match.scrutinee, interp);
            for (usz i = 0; i < expr.match.clause_count; i++) {
                expr.match.clauses[i].result = expand_macros_in_expr(expr.match.clauses[i].result, interp);
            }
            return true;
        case E_MODULE:
            expand_macros_in_sequence(expr.module_expr.body, expr.module_expr.body_count, interp);
            return true;
        default:
            return false;
    }
}

/**
 * expand_macros_in_expr — Pre-JIT macro expansion pass.
 *
 * Walks an AST and expands macros before JIT compilation. Within E_BEGIN
 * blocks, processes sequentially: E_DEFMACRO children are evaluated to register
 * the macro, then subsequent E_CALL children are expanded if they invoke a
 * known macro.
 *
 * Returns the (possibly transformed) expression. Mutates the tree in-place
 * where possible (e.g. replacing E_BEGIN children).
 */
fn Expr* expand_macros_in_expr(Expr* expr, Interp* interp) {
    if (expr == null) return expr;

    switch (expr.tag) {
        case E_BEGIN:
            expand_macros_in_sequence(expr.begin.exprs, expr.begin.expr_count, interp);
            return expr;
        case E_CALL: {
            if (Expr* expanded = expand_macro_call_site(expr, interp)) return expanded;
            expand_macros_call_children(expr, interp);
            return expr;
        }
        default:
            if (expand_macros_binary(expr, interp)) return expr;
            if (expand_macros_unary(expr, interp)) return expr;
            if (expand_macros_complex(expr, interp)) return expr;
            return expr;
    }
}

/**
 * Check if a symbol is a special form keyword (if, begin, let, lambda, etc.).
 * Special forms are recognized by SymbolId in value_to_expr, so they don't need
 * hygienic resolution — they always produce the correct Expr tag regardless of
 * any user-defined shadowing.
 */
fn bool is_special_form_symbol(SymbolId sym, Interp* interp) {
    return (uint)sym == (uint)interp.sym_if
        || (uint)sym == (uint)interp.sym_begin
        || (uint)sym == (uint)interp.sym_let
        || (uint)sym == (uint)interp.sym_lambda
        || (uint)sym == (uint)interp.sym_define
        || (uint)sym == (uint)interp.sym_quote
        || (uint)sym == (uint)interp.sym_set
        || (uint)sym == (uint)interp.sym_and
        || (uint)sym == (uint)interp.sym_or
        || (uint)sym == (uint)interp.sym_reset
        || (uint)sym == (uint)interp.sym_shift
        || (uint)sym == (uint)interp.sym_signal
        || (uint)sym == (uint)interp.sym_handle
        || (uint)sym == (uint)interp.sym_match
        || (uint)sym == (uint)interp.sym_quasiquote
        || (uint)sym == (uint)interp.sym_unquote
        || (uint)sym == (uint)interp.sym_unquote_splicing
        || (uint)sym == (uint)interp.sym_defmacro
        || (uint)sym == (uint)interp.sym_dotdot
        || (uint)sym == (uint)interp.sym_macro
        || (uint)sym == (uint)interp.sym_true
        || (uint)sym == (uint)interp.sym_false
        || (uint)sym == (uint)interp.sym_module
        || (uint)sym == (uint)interp.sym_import
        || (uint)sym == (uint)interp.sym_export;
}

/**
 * Expand a pattern-based macro.
 * Converts macro call args to a value list, tries each clause's pattern,
 * and on first match, expands the template with bindings.
 */
fn EvalResult expand_pattern_macro(MacroDef* mdef, Expr* call_expr, Interp* interp) {
    // Convert each arg Expr* to Value* and build into a cons list
    usz arg_count = call_expr.call.arg_count;
    Value* args_list = make_nil(interp);
    // Build right-to-left to get proper list order
    for (usz i = arg_count; i > 0; i--) {
        Value* arg_val = expr_to_value(call_expr.call.args[i - 1], interp);
        args_list = make_cons(interp, arg_val, args_list);
    }

    // Try each clause
    for (usz i = 0; i < mdef.clause_count; i++) {
        MatchResult match_result = match_pattern(mdef.clauses[i].pattern, args_list, interp);
        if (match_result.matched) {
            // Expand template with bindings; fresh gensym table per expansion
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
    // Check if already mapped
    for (usz i = 0; i < table.count; i++) {
        if ((uint)table.mappings[i].original == (uint)original) {
            return table.mappings[i].generated;
        }
    }
    // Create new gensym
    char[32] buf;
    usz len = 0;
    buf[0] = 'g'; buf[1] = '#'; len = 2;
    // Convert counter to string
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
    // Store mapping
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
            // 1. Check if this symbol is bound in the pattern — substitute
            for (usz i = 0; i < bindings.binding_count; i++) {
                if ((uint)bindings.bindings[i].name == (uint)tmpl.sym_val) {
                    return bindings.bindings[i].value;
                }
            }
            // 2. Check for auto-gensym (symbol ending with #) — generate unique name
            char[] sym_name = interp.symbols.get_name(tmpl.sym_val);
            if (sym_name.len > 0 && sym_name[sym_name.len - 1] == '#') {
                SymbolId gsym = lookup_or_create_gensym(tmpl.sym_val, gensyms, interp);
                return make_symbol(interp, gsym);
            }
            // 3. Hygienic resolution: check captured definition-time bindings.
            //    If this symbol was bound at macro definition time, embed the
            //    snapshotted value directly to prevent expansion-site shadowing.
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
            // INT, STRING, NIL — return as-is
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

    // Check for splice: (.. var ...)
    if (head != null && head.tag == SYMBOL && head.sym_val == interp.sym_dotdot) {
        // Next element is the variable to splice
        Value* rest_after_dotdot = list.cons_val.cdr;
        if (is_cons(rest_after_dotdot)) {
            Value* var_sym = rest_after_dotdot.cons_val.car;
            Value* remaining = rest_after_dotdot.cons_val.cdr;

            // Look up the variable in bindings
            Value* spliced = make_nil(interp);
            if (var_sym != null && var_sym.tag == SYMBOL) {
                for (usz i = 0; i < bindings.binding_count; i++) {
                    if ((uint)bindings.bindings[i].name == (uint)var_sym.sym_val) {
                        spliced = bindings.bindings[i].value;
                        break;
                    }
                }
            }

            // Expand the rest of the template
            Value* expanded_rest = expand_template_list(remaining, bindings, interp, gensyms, captured, captured_count);

            // Append spliced list to expanded_rest
            return append_values(spliced, expanded_rest, interp);
        }
    }

    // Normal element: expand car and recurse on cdr
    Value* expanded_car = expand_template(head, bindings, interp, gensyms, captured, captured_count);
    Value* expanded_cdr = expand_template_list(list.cons_val.cdr, bindings, interp, gensyms, captured, captured_count);
    return make_cons(interp, expanded_car, expanded_cdr);
}

/**
 * Append two value lists.
 */
fn Value* append_values(Value* a, Value* b, Interp* interp, usz depth = 0) {
    if (is_nil(a)) return b;
    if (!is_cons(a)) return b;  // not a proper list, just return b
    if (depth >= 10000) return make_nil(interp);
    return make_cons(interp, a.cons_val.car, append_values(a.cons_val.cdr, b, interp, depth + 1));
}

// =============================================================================
// SECTION 2.7: MODULE SYSTEM
// =============================================================================

/**
 * Find a module by name in the module table.
 */
fn Module* find_module(SymbolId name, Interp* interp) {
    usz hash_slot = (usz)name % interp.module_hash_capacity;
    for (usz probe = 0; probe < interp.module_hash_capacity; probe++) {
        usz idx = interp.module_hash_index[hash_slot];
        if (idx == usz.max) return null;  // Empty slot — not found
        if ((uint)interp.modules[idx].name == (uint)name) {
            return &interp.modules[idx];
        }
        hash_slot = (hash_slot + 1) % interp.module_hash_capacity;
    }
    return null;
}

// Module eval/import — handled by JIT (jit.c3)
