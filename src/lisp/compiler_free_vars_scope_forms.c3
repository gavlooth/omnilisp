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
    } else if ((uint)expr.lambda.param != 0xFFFFFFFF) {
        new_bound.push(expr.lambda.param);
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
        clause_bound.free();
    }
}

fn void Compiler.find_free_vars_call(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    self.find_free_vars(expr.call.func, bound_vars, free_vars, enclosing_scope);
    for (usz i = 0; i < expr.call.arg_count; i++) {
        self.find_free_vars(expr.call.args[i], bound_vars, free_vars, enclosing_scope);
    }
}

fn void Compiler.find_free_vars_path(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    // First segment is a variable reference
    SymbolId path_name = expr.path.segments[0];
    foreach (b : *bound_vars) {
        if ((uint)b == (uint)path_name) return;
    }
    // Check enclosing scope before is_primitive (enclosing var shadows global)
    if (enclosing_scope != null) {
        foreach (e : *enclosing_scope) {
            if ((uint)e == (uint)path_name) {
                foreach (f : *free_vars) {
                    if ((uint)f == (uint)path_name) return;
                }
                free_vars.push(path_name);
                return;
            }
        }
    }
    if (self.is_primitive(path_name)) return;
    foreach (f : *free_vars) {
        if ((uint)f == (uint)path_name) return;
    }
    free_vars.push(path_name);
}

fn void Compiler.find_free_vars_set(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    // The variable being set might be free
    SymbolId set_name = expr.set_expr.name;
    foreach (b : *bound_vars) {
        if ((uint)b == (uint)set_name) {
            // Bound locally, just analyze the value
            self.find_free_vars(expr.set_expr.value, bound_vars, free_vars, enclosing_scope);
            return;
        }
    }
    // Check enclosing scope before is_primitive (enclosing var shadows global)
    if (enclosing_scope != null) {
        foreach (e : *enclosing_scope) {
            if ((uint)e == (uint)set_name) {
                bool set_enc_free = false;
                foreach (f : *free_vars) {
                    if ((uint)f == (uint)set_name) { set_enc_free = true; break; }
                }
                if (!set_enc_free) {
                    free_vars.push(set_name);
                }
                self.find_free_vars(expr.set_expr.value, bound_vars, free_vars, enclosing_scope);
                return;
            }
        }
    }
    if (self.is_primitive(set_name)) {
        self.find_free_vars(expr.set_expr.value, bound_vars, free_vars, enclosing_scope);
        return;
    }
    // Check if already in free vars
    bool set_already_free = false;
    foreach (f : *free_vars) {
        if ((uint)f == (uint)set_name) { set_already_free = true; break; }
    }
    if (!set_already_free) {
        free_vars.push(set_name);
    }
    self.find_free_vars(expr.set_expr.value, bound_vars, free_vars, enclosing_scope);
}
