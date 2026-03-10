module lisp;

import std::collections::list;
import main;
// =============================================================================
// SECTION 4: FREE VARIABLE ANALYSIS (dispatch + qq traversal)
// =============================================================================

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
        case E_BLOCK:
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
