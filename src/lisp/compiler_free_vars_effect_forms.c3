module lisp;

import std::collections::list;
import main;
// =============================================================================
// SECTION 4: FREE VARIABLE ANALYSIS (effect/control form walkers)
// =============================================================================

fn void Compiler.find_free_vars_shift(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    // k_name is bound in the body
    List{SymbolId} shift_bound;
    foreach (b : *bound_vars) {
        shift_bound.push(b);
    }
    shift_bound.push(expr.shift.k_name);
    self.find_free_vars(expr.shift.body, &shift_bound, free_vars, enclosing_scope);
    shift_bound.free();
}

fn void Compiler.find_free_vars_resolve(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    self.find_free_vars(expr.resolve.value, bound_vars, free_vars, enclosing_scope);
    bool k_bound = false;
    foreach (b : *bound_vars) {
        if ((uint)b == (uint)self.interp.sym__k) { k_bound = true; break; }
    }
    if (!k_bound && enclosing_scope != null) {
        foreach (e : *enclosing_scope) {
            if ((uint)e == (uint)self.interp.sym__k) {
                bool already_free = false;
                foreach (f : *free_vars) {
                    if ((uint)f == (uint)self.interp.sym__k) { already_free = true; break; }
                }
                if (!already_free) free_vars.push(self.interp.sym__k);
                break;
            }
        }
    }
}

fn void Compiler.find_free_vars_handle(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    self.find_free_vars(expr.handle.body, bound_vars, free_vars, enclosing_scope);
    for (usz i = 0; i < expr.handle.clause_count; i++) {
        if (expr.handle.clauses[i].has_effect_tag_expr) {
            self.find_free_vars(expr.handle.clauses[i].effect_tag_expr, bound_vars, free_vars, enclosing_scope);
        }
        // k_name and arg_name are bound in handler body
        List{SymbolId} handle_bound;
        foreach (b : *bound_vars) {
            handle_bound.push(b);
        }
        handle_bound.push(expr.handle.clauses[i].k_name);
        handle_bound.push(expr.handle.clauses[i].arg_name);
        self.find_free_vars(expr.handle.clauses[i].handler_body, &handle_bound, free_vars, enclosing_scope);
        handle_bound.free();
    }
}
