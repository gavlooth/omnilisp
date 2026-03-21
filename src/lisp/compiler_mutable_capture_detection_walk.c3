module lisp;

import std::io;
import std::collections::list;
import main;

fn bool Compiler.has_set_on(Compiler* self, SymbolId name, Expr* expr) {
    if (expr == null) return false;
    switch (expr.tag) {
        case E_SET:
            if ((uint)expr.set_expr.name == (uint)name) return true;
            return self.has_set_on(name, expr.set_expr.value);
        case E_APP:
            return self.has_set_on(name, expr.app.func) || self.has_set_on(name, expr.app.arg);
        case E_IF:
            return self.has_set_on(name, expr.if_expr.test) ||
                   self.has_set_on(name, expr.if_expr.then_branch) ||
                   self.has_set_on(name, expr.if_expr.else_branch);
        case E_LET:
            return self.has_set_on(name, expr.let_expr.init) || self.has_set_on(name, expr.let_expr.body);
        case E_LAMBDA:
            return self.has_set_on(name, expr.lambda.body);
        case E_BLOCK:
            for (usz i = 0; i < expr.block.expr_count; i++) {
                if (self.has_set_on(name, expr.block.exprs[i])) return true;
            }
            return false;
        case E_CALL:
            if (self.has_set_on(name, expr.call.func)) return true;
            for (usz i = 0; i < expr.call.arg_count; i++) {
                if (self.has_set_on(name, expr.call.args[i])) return true;
            }
            return false;
        case E_AND:
            return self.has_set_on(name, expr.and_expr.left) || self.has_set_on(name, expr.and_expr.right);
        case E_OR:
            return self.has_set_on(name, expr.or_expr.left) || self.has_set_on(name, expr.or_expr.right);
        case E_MATCH:
            if (self.has_set_on(name, expr.match.scrutinee)) return true;
            for (usz i = 0; i < expr.match.clause_count; i++) {
                if (self.has_set_on(name, expr.match.clauses[i].result)) return true;
            }
            return false;
        default:
            return false;
    }
}

fn bool Compiler.is_captured_by_nested_lambda(Compiler* self, SymbolId name, Expr* expr) {
    if (expr == null) return false;
    switch (expr.tag) {
        case E_LAMBDA:
            // Check if this lambda captures name as a free variable
            List{SymbolId} bound;
            if ((uint)expr.lambda.param != 0xFFFFFFFF) {
                bound.push(expr.lambda.param);
            }
            List{SymbolId} free;
            self.find_free_vars(expr.lambda.body, &bound, &free, null);
            bool captured = false;
            foreach (f : free) {
                if ((uint)f == (uint)name) { captured = true; break; }
            }
            bound.free();
            free.free();
            if (captured) return true;
            // Also check deeper nesting
            return self.is_captured_by_nested_lambda(name, expr.lambda.body);
        case E_APP:
            return self.is_captured_by_nested_lambda(name, expr.app.func) ||
                   self.is_captured_by_nested_lambda(name, expr.app.arg);
        case E_IF:
            return self.is_captured_by_nested_lambda(name, expr.if_expr.test) ||
                   self.is_captured_by_nested_lambda(name, expr.if_expr.then_branch) ||
                   self.is_captured_by_nested_lambda(name, expr.if_expr.else_branch);
        case E_LET:
            return self.is_captured_by_nested_lambda(name, expr.let_expr.init) ||
                   self.is_captured_by_nested_lambda(name, expr.let_expr.body);
        case E_BLOCK:
            for (usz i = 0; i < expr.block.expr_count; i++) {
                if (self.is_captured_by_nested_lambda(name, expr.block.exprs[i])) return true;
            }
            return false;
        case E_CALL:
            if (self.is_captured_by_nested_lambda(name, expr.call.func)) return true;
            for (usz i = 0; i < expr.call.arg_count; i++) {
                if (self.is_captured_by_nested_lambda(name, expr.call.args[i])) return true;
            }
            return false;
        case E_AND:
            return self.is_captured_by_nested_lambda(name, expr.and_expr.left) ||
                   self.is_captured_by_nested_lambda(name, expr.and_expr.right);
        case E_OR:
            return self.is_captured_by_nested_lambda(name, expr.or_expr.left) ||
                   self.is_captured_by_nested_lambda(name, expr.or_expr.right);
        case E_MATCH:
            if (self.is_captured_by_nested_lambda(name, expr.match.scrutinee)) return true;
            for (usz i = 0; i < expr.match.clause_count; i++) {
                if (self.is_captured_by_nested_lambda(name, expr.match.clauses[i].result)) return true;
            }
            return false;
        case E_SET:
            return self.is_captured_by_nested_lambda(name, expr.set_expr.value);
        default:
            return false;
    }
}
