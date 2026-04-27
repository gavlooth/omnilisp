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
    if (self.has_error) return;
    List{SymbolId} empty;
    List{bool} empty_mutable;
    self.scan_lambdas_with_scope(expr, &empty, &empty_mutable);
    empty_mutable.free();
    empty.free();
}

// Returns true if any E_LAMBDA was found in the subtree (replaces body_creates_closure)
// --- scan_lambdas_with_scope helpers ---

fn void Compiler.copy_scan_scope(Compiler* self, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable, List{SymbolId}* out_bound, List{bool}* out_mutable) {
    foreach (b : *enclosing_bound) {
        out_bound.push(b);
    }
    foreach (m : *enclosing_mutable) {
        out_mutable.push(m);
    }
}

fn void Compiler.mark_scan_bindings_immutable(Compiler* self, List{SymbolId}* bound, List{bool}* mutable_flags) {
    while (mutable_flags.len() < bound.len()) {
        mutable_flags.push(false);
    }
}

fn bool Compiler.scan_lambdas_let(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable) {
    List{SymbolId} let_scope;
    List{bool} let_mutable;
    self.copy_scan_scope(enclosing_bound, enclosing_mutable, &let_scope, &let_mutable);
    let_scope.push(expr.let_expr.name);
    let_mutable.push(self.is_let_binding_mutable_capture(expr));
    bool let_found;
    if (expr.let_expr.is_recursive) {
        let_found = self.scan_lambdas_with_scope(expr.let_expr.init, &let_scope, &let_mutable);
    } else {
        let_found = self.scan_lambdas_with_scope(expr.let_expr.init, enclosing_bound, enclosing_mutable);
    }
    bool let_body = self.scan_lambdas_with_scope(expr.let_expr.body, &let_scope, &let_mutable);
    let_mutable.free();
    let_scope.free();
    return let_found || let_body;
}

fn bool Compiler.scan_lambdas_call(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable) {
    bool c_found = self.scan_lambdas_with_scope(expr.call.func, enclosing_bound, enclosing_mutable);
    for (usz i = 0; i < expr.call.arg_count; i++) {
        c_found |= self.scan_lambdas_with_scope(expr.call.args[i], enclosing_bound, enclosing_mutable);
    }
    return c_found;
}

fn bool Compiler.scan_lambdas_binary_expr(Compiler* self, Expr* left, Expr* right, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable) {
    bool left_found = self.scan_lambdas_with_scope(left, enclosing_bound, enclosing_mutable);
    return self.scan_lambdas_with_scope(right, enclosing_bound, enclosing_mutable) || left_found;
}

fn bool Compiler.scan_lambdas_if_expr(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable) {
    bool test_found = self.scan_lambdas_with_scope(expr.if_expr.test, enclosing_bound, enclosing_mutable);
    bool then_found = self.scan_lambdas_with_scope(expr.if_expr.then_branch, enclosing_bound, enclosing_mutable);
    return self.scan_lambdas_with_scope(expr.if_expr.else_branch, enclosing_bound, enclosing_mutable) || test_found || then_found;
}

fn bool Compiler.scan_lambdas_pattern(Compiler* self, Pattern* pat, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable) {
    if (pat == null) return false;

    switch (pat.tag) {
        case PAT_CONS:
            return self.scan_lambdas_pattern(pat.car_pat, enclosing_bound, enclosing_mutable)
                || self.scan_lambdas_pattern(pat.cdr_pat, enclosing_bound, enclosing_mutable);

        case PAT_CONSTRUCTOR:
            bool ctor_found = false;
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                ctor_found |= self.scan_lambdas_pattern(pat.ctor_sub_patterns[i], enclosing_bound, enclosing_mutable);
            }
            return ctor_found;

        case PAT_SEQ:
            bool seq_found = false;
            for (usz i = 0; i < pat.elem_count; i++) {
                seq_found |= self.scan_lambdas_pattern(pat.elements[i], enclosing_bound, enclosing_mutable);
            }
            return seq_found;

        case PAT_AS:
            return self.scan_lambdas_pattern(pat.as_sub, enclosing_bound, enclosing_mutable);

        case PAT_GUARD:
            bool sub_found = self.scan_lambdas_pattern(pat.guard_sub, enclosing_bound, enclosing_mutable);
            List{SymbolId} guard_scope;
            List{bool} guard_mutable;
            self.copy_scan_scope(enclosing_bound, enclosing_mutable, &guard_scope, &guard_mutable);
            if (pat.guard_sub != null) {
                self.collect_pattern_bindings(pat.guard_sub, &guard_scope);
                self.mark_scan_bindings_immutable(&guard_scope, &guard_mutable);
            }
            bool pred_found = self.scan_lambdas_with_scope(pat.guard_pred, &guard_scope, &guard_mutable);
            guard_mutable.free();
            guard_scope.free();
            return sub_found || pred_found;

        case PAT_WILDCARD:
        case PAT_VAR:
        case PAT_LIT:
        case PAT_QUOTE:
        case PAT_DICT:
            return false;

        default:
            self.set_compile_error("compiler: unknown pattern tag in lambda scan");
            return false;
    }
}

fn bool Compiler.scan_lambdas_match_expr(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable) {
    bool found = self.scan_lambdas_with_scope(expr.match.scrutinee, enclosing_bound, enclosing_mutable);
    for (usz i = 0; i < expr.match.clause_count; i++) {
        found |= self.scan_lambdas_pattern(expr.match.clauses[i].pattern, enclosing_bound, enclosing_mutable);

        List{SymbolId} clause_scope;
        List{bool} clause_mutable;
        self.copy_scan_scope(enclosing_bound, enclosing_mutable, &clause_scope, &clause_mutable);
        self.collect_pattern_bindings(expr.match.clauses[i].pattern, &clause_scope);
        self.mark_scan_bindings_immutable(&clause_scope, &clause_mutable);
        found |= self.scan_lambdas_with_scope(expr.match.clauses[i].result, &clause_scope, &clause_mutable);
        clause_mutable.free();
        clause_scope.free();
    }
    return found;
}

fn bool Compiler.scan_lambdas_block_expr(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable) {
    bool found = false;
    for (usz i = 0; i < expr.block.expr_count; i++) {
        found |= self.scan_lambdas_with_scope(expr.block.exprs[i], enclosing_bound, enclosing_mutable);
    }
    return found;
}

fn bool Compiler.scan_lambdas_module_expr(Compiler* self, Expr* expr) {
    bool found = false;
    for (usz i = 0; i < expr.module_expr.body_count; i++) {
        List{SymbolId} mod_empty;
        List{bool} mod_empty_mutable;
        found |= self.scan_lambdas_with_scope(expr.module_expr.body[i], &mod_empty, &mod_empty_mutable);
        mod_empty_mutable.free();
        mod_empty.free();
    }
    return found;
}

fn bool Compiler.scan_lambdas_with_module_expr(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable) {
    if (expr.with_module == null) return false;
    bool found = false;
    for (usz i = 0; i < expr.with_module.body_count; i++) {
        found |= self.scan_lambdas_with_scope(expr.with_module.body[i], enclosing_bound, enclosing_mutable);
    }
    return found;
}

fn bool Compiler.scan_lambdas_with_scope(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable) {
    if (self.has_error) return false;
    if (expr == null) return false;
    switch (expr.tag) {
        case E_LAMBDA: return self.scan_lambdas_lambda(expr, enclosing_bound, enclosing_mutable);
        case E_APP: return self.scan_lambdas_binary_expr(expr.app.func, expr.app.arg, enclosing_bound, enclosing_mutable);
        case E_IF: return self.scan_lambdas_if_expr(expr, enclosing_bound, enclosing_mutable);
        case E_LET: return self.scan_lambdas_let(expr, enclosing_bound, enclosing_mutable);
        case E_DEFINE: return self.scan_lambdas_with_scope(expr.define.value, enclosing_bound, enclosing_mutable);
        case E_AND: return self.scan_lambdas_binary_expr(expr.and_expr.left, expr.and_expr.right, enclosing_bound, enclosing_mutable);
        case E_OR: return self.scan_lambdas_binary_expr(expr.or_expr.left, expr.or_expr.right, enclosing_bound, enclosing_mutable);
        case E_MATCH: return self.scan_lambdas_match_expr(expr, enclosing_bound, enclosing_mutable);
        case E_CALL: return self.scan_lambdas_call(expr, enclosing_bound, enclosing_mutable);
        case E_INDEX: return self.scan_lambdas_binary_expr(expr.index.collection, expr.index.index, enclosing_bound, enclosing_mutable);
        case E_RESET: return self.scan_lambdas_reset(expr, enclosing_bound, enclosing_mutable);
        case E_SHIFT: return self.scan_lambdas_shift(expr, enclosing_bound, enclosing_mutable);
        case E_PERFORM:
            if (expr.perform.has_tag_expr &&
                self.scan_lambdas_with_scope(expr.perform.tag_expr, enclosing_bound, enclosing_mutable)) {
                return true;
            }
            return self.scan_lambdas_with_scope(expr.perform.arg, enclosing_bound, enclosing_mutable);
        case E_RESOLVE: return self.scan_lambdas_with_scope(expr.resolve.value, enclosing_bound, enclosing_mutable);
        case E_HANDLE: return self.scan_lambdas_handle(expr, enclosing_bound, enclosing_mutable);
        case E_BLOCK: return self.scan_lambdas_block_expr(expr, enclosing_bound, enclosing_mutable);
        case E_SET: return self.scan_lambdas_with_scope(expr.set_expr.value, enclosing_bound, enclosing_mutable);
        case E_QUASIQUOTE: return self.scan_lambdas_with_scope(expr.quasiquote.body, enclosing_bound, enclosing_mutable);
        case E_UNQUOTE: return self.scan_lambdas_with_scope(expr.unquote.body, enclosing_bound, enclosing_mutable);
        case E_UNQUOTE_SPLICING: return self.scan_lambdas_with_scope(expr.unquote_splicing.body, enclosing_bound, enclosing_mutable);
        case E_MODULE: return self.scan_lambdas_module_expr(expr);
        case E_WITH_MODULE: return self.scan_lambdas_with_module_expr(expr, enclosing_bound, enclosing_mutable);
        case E_LIT:
        case E_VAR:
        case E_QUOTE:
        case E_PATH:
        case E_DEFMACRO:
        case E_IMPORT:
        case E_EXPORT_FROM:
        case E_FFI_LIB:
        case E_FFI_FN:
        case E_DEFTYPE:
        case E_DEFABSTRACT:
        case E_DEFUNION:
        case E_DEFALIAS:
        case E_DEFEFFECT:
            return false;
        default:
            self.set_compile_error("compiler: unknown expression tag in lambda scan");
            return false;
    }
}
