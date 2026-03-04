module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 4: FREE VARIABLE ANALYSIS (walkers)
// =============================================================================
// --- find_free_vars helpers ---

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

/**
 * Find free variables in an expression that need to be captured.
 * bound_vars contains variables that are bound in the current scope.
 */
fn void Compiler.find_free_vars_pair(Compiler* self, Expr* a, Expr* b, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    self.find_free_vars(a, bound_vars, free_vars, enclosing_scope);
    self.find_free_vars(b, bound_vars, free_vars, enclosing_scope);
}

fn void Compiler.find_free_vars_triple(Compiler* self, Expr* a, Expr* b, Expr* c, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    self.find_free_vars(a, bound_vars, free_vars, enclosing_scope);
    self.find_free_vars(b, bound_vars, free_vars, enclosing_scope);
    self.find_free_vars(c, bound_vars, free_vars, enclosing_scope);
}

fn void Compiler.find_free_vars_list(Compiler* self, Expr** exprs, usz count, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    for (usz i = 0; i < count; i++) {
        self.find_free_vars(exprs[i], bound_vars, free_vars, enclosing_scope);
    }
}

fn void Compiler.find_free_vars_unary_form(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    switch (expr.tag) {
        case E_DEFINE:
            self.find_free_vars(expr.define.value, bound_vars, free_vars, enclosing_scope);
        case E_RESET:
            self.find_free_vars(expr.reset.body, bound_vars, free_vars, enclosing_scope);
        case E_PERFORM:
            self.find_free_vars(expr.perform.arg, bound_vars, free_vars, enclosing_scope);
        case E_UNQUOTE:
            self.find_free_vars(expr.unquote.body, bound_vars, free_vars, enclosing_scope);
        case E_UNQUOTE_SPLICING:
            self.find_free_vars(expr.unquote_splicing.body, bound_vars, free_vars, enclosing_scope);
        default:
            return;
    }
}

fn void Compiler.find_free_vars_binary_form(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    switch (expr.tag) {
        case E_APP:
            self.find_free_vars_pair(expr.app.func, expr.app.arg, bound_vars, free_vars, enclosing_scope);
        case E_AND:
            self.find_free_vars_pair(expr.and_expr.left, expr.and_expr.right, bound_vars, free_vars, enclosing_scope);
        case E_OR:
            self.find_free_vars_pair(expr.or_expr.left, expr.or_expr.right, bound_vars, free_vars, enclosing_scope);
        case E_INDEX:
            self.find_free_vars_pair(expr.index.collection, expr.index.index, bound_vars, free_vars, enclosing_scope);
        case E_IF:
            self.find_free_vars_triple(expr.if_expr.test, expr.if_expr.then_branch, expr.if_expr.else_branch, bound_vars, free_vars, enclosing_scope);
        default:
            return;
    }
}

fn void Compiler.find_free_vars_complex_form(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    switch (expr.tag) {
        case E_LAMBDA:
            self.find_free_vars_lambda(expr, bound_vars, free_vars, enclosing_scope);
        case E_LET:
            self.find_free_vars_let(expr, bound_vars, free_vars, enclosing_scope);
        case E_MATCH:
            self.find_free_vars_match(expr, bound_vars, free_vars, enclosing_scope);
        case E_CALL:
            self.find_free_vars_call(expr, bound_vars, free_vars, enclosing_scope);
        case E_PATH:
            self.find_free_vars_path(expr, bound_vars, free_vars, enclosing_scope);
        case E_SHIFT:
            self.find_free_vars_shift(expr, bound_vars, free_vars, enclosing_scope);
        case E_RESOLVE:
            self.find_free_vars_resolve(expr, bound_vars, free_vars, enclosing_scope);
        case E_HANDLE:
            self.find_free_vars_handle(expr, bound_vars, free_vars, enclosing_scope);
        case E_BEGIN:
            self.find_free_vars_list(expr.begin.exprs, expr.begin.expr_count, bound_vars, free_vars, enclosing_scope);
        case E_QUASIQUOTE:
            self.find_free_vars_in_qq(expr.quasiquote.body, bound_vars, free_vars, enclosing_scope);
        case E_MODULE:
            self.find_free_vars_list(expr.module_expr.body, expr.module_expr.body_count, bound_vars, free_vars, enclosing_scope);
        case E_SET:
            self.find_free_vars_set(expr, bound_vars, free_vars, enclosing_scope);
        default:
            return;
    }
}

fn void Compiler.find_free_vars(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    if (expr == null) return;

    switch (expr.tag) {
        case E_LIT:
        case E_QUOTE:
        case E_DEFMACRO:
        case E_IMPORT:
        case E_EXPORT_FROM:
            return;

        case E_VAR:
            self.find_free_vars_var(expr, bound_vars, free_vars, enclosing_scope);

        case E_DEFINE:
        case E_RESET:
        case E_PERFORM:
        case E_UNQUOTE:
        case E_UNQUOTE_SPLICING:
            self.find_free_vars_unary_form(expr, bound_vars, free_vars, enclosing_scope);

        case E_APP:
        case E_IF:
        case E_AND:
        case E_OR:
        case E_INDEX:
            self.find_free_vars_binary_form(expr, bound_vars, free_vars, enclosing_scope);

        case E_LAMBDA:
        case E_LET:
        case E_MATCH:
        case E_CALL:
        case E_PATH:
        case E_SHIFT:
        case E_RESOLVE:
        case E_HANDLE:
        case E_BEGIN:
        case E_QUASIQUOTE:
        case E_MODULE:
        case E_SET:
            self.find_free_vars_complex_form(expr, bound_vars, free_vars, enclosing_scope);

        default:
            return;
    }
}

/**
 * Traverse quasiquote body, only descending into unquote/unquote-splicing for free vars.
 * In quasiquote, bare symbols are literals — only unquoted expressions reference variables.
 */
fn void Compiler.find_free_vars_in_qq(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    if (expr == null) return;
    switch (expr.tag) {
        case E_UNQUOTE:
            // Unquoted expression — evaluate normally, so scan for free vars
            self.find_free_vars(expr.unquote.body, bound_vars, free_vars, enclosing_scope);
        case E_UNQUOTE_SPLICING:
            self.find_free_vars(expr.unquote_splicing.body, bound_vars, free_vars, enclosing_scope);
        case E_CALL:
            // Lists in qq template are parsed as E_CALL — traverse to find unquotes
            self.find_free_vars_in_qq(expr.call.func, bound_vars, free_vars, enclosing_scope);
            for (usz i = 0; i < expr.call.arg_count; i++) {
                self.find_free_vars_in_qq(expr.call.args[i], bound_vars, free_vars, enclosing_scope);
            }
        case E_APP:
            self.find_free_vars_in_qq(expr.app.func, bound_vars, free_vars, enclosing_scope);
            self.find_free_vars_in_qq(expr.app.arg, bound_vars, free_vars, enclosing_scope);
        case E_QUASIQUOTE:
            // Nested quasiquote — still only scan for unquotes
            self.find_free_vars_in_qq(expr.quasiquote.body, bound_vars, free_vars, enclosing_scope);
        default:
            // E_VAR, E_LIT, etc. in qq context are literal — no free vars
            return;
    }
}
