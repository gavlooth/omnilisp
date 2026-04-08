module lisp;

import std::collections::list;
import main;

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

        for (usz i = 0; i < pat_vars.len(); i++) {
            if ((uint)(*pat_vars)[i] == (uint)sym) return;
        }

        char[] sym_name = interp.symbols.get_name(sym);
        if (sym_name.len > 0 && sym_name[sym_name.len - 1] == '#') return;

        if (is_special_form_symbol(sym, interp)) return;

        for (usz i = 0; i < bindings.len(); i++) {
            if ((uint)(*bindings)[i].sym == (uint)sym) return;
        }

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
 * (define [macro] name (syntax-match (pattern1 (template ...)) ...))
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

    for (usz i = 0; i < expr.define_macro.clause_count; i++) {
        List{SymbolId} pat_vars;
        defer pat_vars.free();
        collect_pattern_vars(new_def.clauses[i].pattern, &pat_vars);

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

    usz hash_slot = (usz)new_def.name % interp.macro_hash_capacity;
    for (;;) {
        usz existing = interp.macro_hash_index[hash_slot];
        if (existing == usz.max) {
            interp.macro_hash_index[hash_slot] = macro_idx;
            break;
        }
        if ((uint)interp.macro_table[existing].name == (uint)new_def.name) {
            interp.macro_hash_index[hash_slot] = macro_idx;
            break;
        }
        hash_slot = (hash_slot + 1) % interp.macro_hash_capacity;
    }
    return eval_ok(make_nil(interp));
}

/**
 * Check if a symbol is a special form keyword (if, block, let, lambda, etc.).
 * Special forms are recognized by SymbolId in value_to_expr, so they don't need
 * hygienic resolution — they always produce the correct Expr tag regardless of
 * any user-defined shadowing.
 */
fn bool is_special_form_symbol(SymbolId sym, Interp* interp) {
    return (uint)sym == (uint)interp.sym_if
        || (uint)sym == (uint)interp.sym_block
        || (uint)sym == (uint)interp.sym_let
        || (uint)sym == (uint)interp.sym_lambda
        || (uint)sym == (uint)interp.sym_define
        || (uint)sym == (uint)interp.sym_quote
        || (uint)sym == (uint)interp.sym_set
        || (uint)sym == (uint)interp.sym_and
        || (uint)sym == (uint)interp.sym_or
        || (uint)sym == (uint)interp.sym_checkpoint
        || (uint)sym == (uint)interp.sym_capture
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
