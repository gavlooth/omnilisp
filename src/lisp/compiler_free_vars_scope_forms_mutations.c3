module lisp;

import std::collections::list;
import main;

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
