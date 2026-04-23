module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 4a1: MUTABLE CAPTURE PRESCAN
// =============================================================================

/**
 * Pre-scan the entire expression tree to identify all mutable-captured variables.
 * Must be called before scan_lambdas so that lambda scanning can exclude
 * mutable-captured variables from closure struct captures.
 */
fn bool Compiler.mutable_capture_contains(Compiler* self, SymbolId sym) {
    foreach (mc : self.mutable_captures) {
        if ((uint)mc == (uint)sym) return true;
    }
    return false;
}

fn void Compiler.add_mutable_capture_if_absent(Compiler* self, SymbolId sym) {
    if (self.mutable_capture_contains(sym)) return;
    self.mutable_captures.push(sym);
}

fn void Compiler.prescan_let_expr(Compiler* self, Expr* expr) {
    // Check if this let-bound variable is mutable-captured.
    if (self.is_mutable_capture(expr.let_expr.name, expr.let_expr.body)) {
        self.add_mutable_capture_if_absent(expr.let_expr.name);
    }
    self.prescan_mutable_captures(expr.let_expr.init);
    self.prescan_mutable_captures(expr.let_expr.body);
}

fn void Compiler.prescan_expr_list(Compiler* self, Expr** exprs, usz count) {
    for (usz i = 0; i < count; i++) {
        self.prescan_mutable_captures(exprs[i]);
    }
}

fn void Compiler.prescan_match_clause_results(Compiler* self, Expr* expr) {
    self.prescan_mutable_captures(expr.match.scrutinee);
    for (usz i = 0; i < expr.match.clause_count; i++) {
        self.prescan_mutable_captures(expr.match.clauses[i].result);
    }
}

fn void Compiler.prescan_handle_clause_bodies(Compiler* self, Expr* expr) {
    self.prescan_mutable_captures(expr.handle.body);
    for (usz i = 0; i < expr.handle.clause_count; i++) {
        self.prescan_mutable_captures(expr.handle.clauses[i].handler_body);
    }
}

fn void Compiler.prescan_mutable_captures(Compiler* self, Expr* expr) {
    if (expr == null) return;
    switch (expr.tag) {
        case E_LET:
            self.prescan_let_expr(expr);
            break;
        case E_LAMBDA:
            self.prescan_mutable_captures(expr.lambda.body);
            break;
        case E_APP:
            self.prescan_mutable_captures(expr.app.func);
            self.prescan_mutable_captures(expr.app.arg);
            break;
        case E_IF:
            self.prescan_mutable_captures(expr.if_expr.test);
            self.prescan_mutable_captures(expr.if_expr.then_branch);
            self.prescan_mutable_captures(expr.if_expr.else_branch);
            break;
        case E_DEFINE:
            self.prescan_mutable_captures(expr.define.value);
            break;
        case E_BLOCK:
            self.prescan_expr_list(expr.block.exprs, expr.block.expr_count);
            break;
        case E_CALL:
            self.prescan_mutable_captures(expr.call.func);
            self.prescan_expr_list(expr.call.args, expr.call.arg_count);
            break;
        case E_AND:
            self.prescan_mutable_captures(expr.and_expr.left);
            self.prescan_mutable_captures(expr.and_expr.right);
            break;
        case E_OR:
            self.prescan_mutable_captures(expr.or_expr.left);
            self.prescan_mutable_captures(expr.or_expr.right);
            break;
        case E_MATCH:
            self.prescan_match_clause_results(expr);
            break;
        case E_SET:
            self.prescan_mutable_captures(expr.set_expr.value);
            break;
        case E_RESET:
            self.prescan_mutable_captures(expr.reset.body);
            break;
        case E_SHIFT:
            self.prescan_mutable_captures(expr.shift.body);
            break;
        case E_HANDLE:
            self.prescan_handle_clause_bodies(expr);
            break;
        case E_PERFORM:
            self.prescan_mutable_captures(expr.perform.arg);
            break;
        case E_RESOLVE:
            self.prescan_mutable_captures(expr.resolve.value);
            break;
        case E_INDEX:
            self.prescan_mutable_captures(expr.index.collection);
            self.prescan_mutable_captures(expr.index.index);
            break;
        case E_QUASIQUOTE:
            self.prescan_mutable_captures(expr.quasiquote.body);
            break;
        case E_UNQUOTE:
            self.prescan_mutable_captures(expr.unquote.body);
            break;
        case E_UNQUOTE_SPLICING:
            self.prescan_mutable_captures(expr.unquote_splicing.body);
            break;
        default:
            return;
    }
}
