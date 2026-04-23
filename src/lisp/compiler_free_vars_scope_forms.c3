module lisp;

import std::collections::list;
import main;
// =============================================================================
// SECTION 4: FREE VARIABLE ANALYSIS (scope/form walkers)
// =============================================================================

fn void Compiler.find_free_vars_var(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    SymbolId name = expr.var_expr.name;
    // Check if bound locally (this lambda's own params/lets)
    foreach (b : *bound_vars) {
        if ((uint)b == (uint)name) return;
    }
    // Check if in enclosing scope (outer lambda params/lets) — must capture,
    // even if the name also exists as a global define
    if (enclosing_scope != null) {
        foreach (e : *enclosing_scope) {
            if ((uint)e == (uint)name) {
                foreach (f : *free_vars) {
                    if ((uint)f == (uint)name) return;
                }
                free_vars.push(name);
                return;
            }
        }
    }
    // Check if it's a global primitive (for delegation, only skip builtins)
    if (self.for_delegation) {
        if (self.is_builtin_primitive(name)) return;
    } else {
        if (self.is_primitive(name)) return;
    }
    // Check if it's a mutable-captured variable (accessed via interpreter env)
    if (self.is_mutable_captured_var(name)) return;
    // Check if already in free vars
    foreach (f : *free_vars) {
        if ((uint)f == (uint)name) return;
    }
    // It's a free variable
    free_vars.push(name);
}

fn void Compiler.find_free_vars_lambda(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    // Add ALL parameters to bound vars (multi-param lambdas have param_count > 1)
    List{SymbolId} new_bound;
    foreach (b : *bound_vars) {
        new_bound.push(b);
    }
    if (expr.lambda.param_count > 1) {
        for (usz pi = 0; pi < expr.lambda.param_count; pi++) {
            new_bound.push(expr.lambda.params[pi]);
        }
    } else if (expr.lambda.param != INVALID_SYMBOL_ID) {
        new_bound.push(expr.lambda.param);
    }
    if (expr.lambda.has_rest && expr.lambda.rest_param != INVALID_SYMBOL_ID) {
        new_bound.push(expr.lambda.rest_param);
    }
    self.find_free_vars(expr.lambda.body, &new_bound, free_vars, enclosing_scope);
    new_bound.free();
}

fn void Compiler.find_free_vars_let(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    // For recursive let, name is bound in both init and body
    List{SymbolId} let_bound;
    foreach (b : *bound_vars) {
        let_bound.push(b);
    }
    if (expr.let_expr.is_recursive) {
        let_bound.push(expr.let_expr.name);
    }
    // Init expression
    self.find_free_vars(expr.let_expr.init, &let_bound, free_vars, enclosing_scope);
    // Body with name bound
    if (!expr.let_expr.is_recursive) {
        let_bound.push(expr.let_expr.name);
    }
    self.find_free_vars(expr.let_expr.body, &let_bound, free_vars, enclosing_scope);
    let_bound.free();
}

fn void Compiler.find_free_vars_match(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    self.find_free_vars(expr.match.scrutinee, bound_vars, free_vars, enclosing_scope);
    for (usz i = 0; i < expr.match.clause_count; i++) {
        // Add pattern bindings to bound vars
        List{SymbolId} clause_bound;
        foreach (b : *bound_vars) {
            clause_bound.push(b);
        }
        self.collect_pattern_bindings(expr.match.clauses[i].pattern, &clause_bound);
        self.find_free_vars(expr.match.clauses[i].result, &clause_bound, free_vars, enclosing_scope);
        // Guard predicates can reference bound variables from the enclosing scope
        self.find_free_vars_in_pattern_guard(expr.match.clauses[i].pattern, bound_vars, free_vars, enclosing_scope);
        clause_bound.free();
    }
}

fn void Compiler.find_free_vars_in_pattern_guard(Compiler* self, Pattern* pat, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    if (pat == null) return;
    switch (pat.tag) {
        case PAT_CONS:
            self.find_free_vars_in_pattern_guard(pat.car_pat, bound_vars, free_vars, enclosing_scope);
            self.find_free_vars_in_pattern_guard(pat.cdr_pat, bound_vars, free_vars, enclosing_scope);
        case PAT_CONSTRUCTOR:
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                self.find_free_vars_in_pattern_guard(pat.ctor_sub_patterns[i], bound_vars, free_vars, enclosing_scope);
            }
        case PAT_SEQ:
            for (usz i = 0; i < pat.elem_count; i++) {
                self.find_free_vars_in_pattern_guard(pat.elements[i], bound_vars, free_vars, enclosing_scope);
            }
        case PAT_AS:
            self.find_free_vars_in_pattern_guard(pat.as_sub, bound_vars, free_vars, enclosing_scope);
        case PAT_GUARD:
            self.find_free_vars(pat.guard_pred, bound_vars, free_vars, enclosing_scope);
            self.find_free_vars_in_pattern_guard(pat.guard_sub, bound_vars, free_vars, enclosing_scope);
        case PAT_VAR:
        case PAT_LIT:
        case PAT_QUOTE:
        case PAT_DICT:
        case PAT_WILDCARD:
            return;
    }
}

fn void Compiler.find_free_vars_call(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    self.find_free_vars(expr.call.func, bound_vars, free_vars, enclosing_scope);
    for (usz i = 0; i < expr.call.arg_count; i++) {
        self.find_free_vars(expr.call.args[i], bound_vars, free_vars, enclosing_scope);
    }
}
