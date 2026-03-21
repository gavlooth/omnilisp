module lisp;

import std::collections::list;
import main;

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
        case E_BLOCK:
            self.find_free_vars_list(expr.block.exprs, expr.block.expr_count, bound_vars, free_vars, enclosing_scope);
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
