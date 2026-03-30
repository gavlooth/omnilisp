module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 5: MAIN COMPILATION (lambda scan + wrapping)
// =============================================================================
/**
 * Scan expression tree for lambdas and register them.
 */
fn void Compiler.scan_lambdas(Compiler* self, Expr* expr) {
    List{SymbolId} empty;
    self.scan_lambdas_with_scope(expr, &empty);
    empty.free();
}

// Returns true if any E_LAMBDA was found in the subtree (replaces body_creates_closure)
// --- scan_lambdas_with_scope helpers ---

fn bool Compiler.scan_lambdas_let(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    List{SymbolId} let_scope;
    foreach (b : *enclosing_bound) {
        let_scope.push(b);
    }
    let_scope.push(expr.let_expr.name);
    bool let_found;
    if (expr.let_expr.is_recursive) {
        let_found = self.scan_lambdas_with_scope(expr.let_expr.init, &let_scope);
    } else {
        let_found = self.scan_lambdas_with_scope(expr.let_expr.init, enclosing_bound);
    }
    bool let_body = self.scan_lambdas_with_scope(expr.let_expr.body, &let_scope);
    let_scope.free();
    return let_found || let_body;
}

fn bool Compiler.scan_lambdas_call(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    bool c_found = self.scan_lambdas_with_scope(expr.call.func, enclosing_bound);
    for (usz i = 0; i < expr.call.arg_count; i++) {
        c_found |= self.scan_lambdas_with_scope(expr.call.args[i], enclosing_bound);
    }
    return c_found;
}

fn bool Compiler.scan_lambdas_binary_expr(Compiler* self, Expr* left, Expr* right, List{SymbolId}* enclosing_bound) {
    bool left_found = self.scan_lambdas_with_scope(left, enclosing_bound);
    return self.scan_lambdas_with_scope(right, enclosing_bound) || left_found;
}

fn bool Compiler.scan_lambdas_if_expr(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    bool test_found = self.scan_lambdas_with_scope(expr.if_expr.test, enclosing_bound);
    bool then_found = self.scan_lambdas_with_scope(expr.if_expr.then_branch, enclosing_bound);
    return self.scan_lambdas_with_scope(expr.if_expr.else_branch, enclosing_bound) || test_found || then_found;
}

fn bool Compiler.scan_lambdas_pattern(Compiler* self, Pattern* pat, List{SymbolId}* enclosing_bound) {
    if (pat == null) return false;

    switch (pat.tag) {
        case PAT_CONS:
            return self.scan_lambdas_pattern(pat.car_pat, enclosing_bound)
                || self.scan_lambdas_pattern(pat.cdr_pat, enclosing_bound);

        case PAT_CONSTRUCTOR:
            bool ctor_found = false;
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                ctor_found |= self.scan_lambdas_pattern(pat.ctor_sub_patterns[i], enclosing_bound);
            }
            return ctor_found;

        case PAT_SEQ:
            bool seq_found = false;
            for (usz i = 0; i < pat.elem_count; i++) {
                seq_found |= self.scan_lambdas_pattern(pat.elements[i], enclosing_bound);
            }
            return seq_found;

        case PAT_AS:
            return self.scan_lambdas_pattern(pat.as_sub, enclosing_bound);

        case PAT_GUARD:
            bool sub_found = self.scan_lambdas_pattern(pat.guard_sub, enclosing_bound);
            List{SymbolId} guard_scope;
            foreach (b : *enclosing_bound) {
                guard_scope.push(b);
            }
            if (pat.guard_sub != null) {
                self.collect_pattern_bindings(pat.guard_sub, &guard_scope);
            }
            bool pred_found = self.scan_lambdas_with_scope(pat.guard_pred, &guard_scope);
            guard_scope.free();
            return sub_found || pred_found;

        default:
            return false;
    }
}

fn bool Compiler.scan_lambdas_match_expr(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    bool found = self.scan_lambdas_with_scope(expr.match.scrutinee, enclosing_bound);
    for (usz i = 0; i < expr.match.clause_count; i++) {
        found |= self.scan_lambdas_pattern(expr.match.clauses[i].pattern, enclosing_bound);

        List{SymbolId} clause_scope;
        foreach (b : *enclosing_bound) {
            clause_scope.push(b);
        }
        self.collect_pattern_bindings(expr.match.clauses[i].pattern, &clause_scope);
        found |= self.scan_lambdas_with_scope(expr.match.clauses[i].result, &clause_scope);
        clause_scope.free();
    }
    return found;
}

fn bool Compiler.scan_lambdas_block_expr(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    bool found = false;
    for (usz i = 0; i < expr.block.expr_count; i++) {
        found |= self.scan_lambdas_with_scope(expr.block.exprs[i], enclosing_bound);
    }
    return found;
}

fn bool Compiler.scan_lambdas_module_expr(Compiler* self, Expr* expr) {
    bool found = false;
    for (usz i = 0; i < expr.module_expr.body_count; i++) {
        List{SymbolId} mod_empty;
        found |= self.scan_lambdas_with_scope(expr.module_expr.body[i], &mod_empty);
        mod_empty.free();
    }
    return found;
}

fn bool Compiler.scan_lambdas_with_scope(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    if (expr == null) return false;
    switch (expr.tag) {
        case E_LAMBDA: return self.scan_lambdas_lambda(expr, enclosing_bound);
        case E_APP: return self.scan_lambdas_binary_expr(expr.app.func, expr.app.arg, enclosing_bound);
        case E_IF: return self.scan_lambdas_if_expr(expr, enclosing_bound);
        case E_LET: return self.scan_lambdas_let(expr, enclosing_bound);
        case E_DEFINE: return self.scan_lambdas_with_scope(expr.define.value, enclosing_bound);
        case E_AND: return self.scan_lambdas_binary_expr(expr.and_expr.left, expr.and_expr.right, enclosing_bound);
        case E_OR: return self.scan_lambdas_binary_expr(expr.or_expr.left, expr.or_expr.right, enclosing_bound);
        case E_MATCH: return self.scan_lambdas_match_expr(expr, enclosing_bound);
        case E_CALL: return self.scan_lambdas_call(expr, enclosing_bound);
        case E_INDEX: return self.scan_lambdas_binary_expr(expr.index.collection, expr.index.index, enclosing_bound);
        case E_PATH: return false;
        case E_RESET: return self.scan_lambdas_reset(expr, enclosing_bound);
        case E_SHIFT: return self.scan_lambdas_shift(expr, enclosing_bound);
        case E_PERFORM:
            if (expr.perform.has_tag_expr &&
                self.scan_lambdas_with_scope(expr.perform.tag_expr, enclosing_bound)) {
                return true;
            }
            return self.scan_lambdas_with_scope(expr.perform.arg, enclosing_bound);
        case E_RESOLVE: return self.scan_lambdas_with_scope(expr.resolve.value, enclosing_bound);
        case E_HANDLE: return self.scan_lambdas_handle(expr, enclosing_bound);
        case E_BLOCK: return self.scan_lambdas_block_expr(expr, enclosing_bound);
        case E_SET: return self.scan_lambdas_with_scope(expr.set_expr.value, enclosing_bound);
        case E_QUASIQUOTE: return self.scan_lambdas_with_scope(expr.quasiquote.body, enclosing_bound);
        case E_UNQUOTE: return self.scan_lambdas_with_scope(expr.unquote.body, enclosing_bound);
        case E_UNQUOTE_SPLICING: return self.scan_lambdas_with_scope(expr.unquote_splicing.body, enclosing_bound);
        case E_DEFMACRO: return false;
        case E_MODULE: return self.scan_lambdas_module_expr(expr);
        case E_IMPORT:
        case E_EXPORT_FROM: return false;
        default: return false;
    }
}
