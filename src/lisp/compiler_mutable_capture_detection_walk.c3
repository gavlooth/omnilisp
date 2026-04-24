module lisp;

import std::io;
import std::collections::list;
import main;

fn bool Compiler.has_set_on_pattern(Compiler* self, SymbolId name, Pattern* pat) {
    if (pat == null) return false;
    switch (pat.tag) {
        case PAT_CONS:
            return self.has_set_on_pattern(name, pat.car_pat)
                || self.has_set_on_pattern(name, pat.cdr_pat);
        case PAT_CONSTRUCTOR:
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                if (self.has_set_on_pattern(name, pat.ctor_sub_patterns[i])) return true;
            }
            return false;
        case PAT_SEQ:
            for (usz i = 0; i < pat.elem_count; i++) {
                if (self.has_set_on_pattern(name, pat.elements[i])) return true;
            }
            return false;
        case PAT_AS:
            return self.has_set_on_pattern(name, pat.as_sub);
        case PAT_GUARD:
            if (pattern_binds_name(pat.guard_sub, name)) {
                return self.has_set_on_pattern(name, pat.guard_sub);
            }
            if (self.has_set_on(name, pat.guard_pred)) return true;
            return self.has_set_on_pattern(name, pat.guard_sub);
        case PAT_VAR:
        case PAT_LIT:
        case PAT_QUOTE:
        case PAT_DICT:
        case PAT_WILDCARD:
            return false;
    }
}

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
            // If the lambda shadows the name, any set! inside refers to the parameter,
            // not the outer variable we're checking for.
            if (expr.lambda.param_count > 1) {
                for (usz pi = 0; pi < expr.lambda.param_count; pi++) {
                    if ((uint)expr.lambda.params[pi] == (uint)name) return false;
                }
            } else if ((uint)expr.lambda.param == (uint)name) {
                return false;
            }
            if (expr.lambda.has_rest && (uint)expr.lambda.rest_param == (uint)name) {
                return false;
            }
            return self.has_set_on(name, expr.lambda.body);
        case E_DEFINE:
            return self.has_set_on(name, expr.define.value);
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
                if (self.has_set_on_pattern(name, expr.match.clauses[i].pattern)) return true;
            }
            return false;
        case E_LIT:
        case E_VAR:
        case E_QUOTE:
        case E_RESET:
        case E_SHIFT:
            return false;
        case E_PERFORM:
            if (expr.perform.has_tag_expr && self.has_set_on(name, expr.perform.tag_expr)) return true;
            return self.has_set_on(name, expr.perform.arg);
        case E_RESOLVE:
            return self.has_set_on(name, expr.resolve.value);
        case E_HANDLE:
            if (self.has_set_on(name, expr.handle.body)) return true;
            for (usz i = 0; i < expr.handle.clause_count; i++) {
                EffectClause* clause = &expr.handle.clauses[i];
                if (clause.has_effect_tag_expr && self.has_set_on(name, clause.effect_tag_expr)) return true;
                if ((uint)clause.k_name == (uint)name || (uint)clause.arg_name == (uint)name) continue;
                if (self.has_set_on(name, clause.handler_body)) return true;
            }
            return false;
        case E_INDEX:
            return self.has_set_on(name, expr.index.collection) || self.has_set_on(name, expr.index.index);
        case E_WITH_MODULE:
            if (expr.with_module == null) return false;
            for (usz i = 0; i < expr.with_module.body_count; i++) {
                if (self.has_set_on(name, expr.with_module.body[i])) return true;
            }
            return false;
        case E_PATH:
        case E_QUASIQUOTE:
        case E_UNQUOTE:
        case E_UNQUOTE_SPLICING:
        case E_DEFMACRO:
        case E_MODULE:
        case E_IMPORT:
        case E_DEFTYPE:
        case E_DEFABSTRACT:
        case E_DEFUNION:
        case E_DEFALIAS:
        case E_DEFEFFECT:
        case E_EXPORT_FROM:
        case E_FFI_LIB:
        case E_FFI_FN:
            return false;
    }
}

fn bool Compiler.is_captured_by_nested_lambda_pattern(Compiler* self, SymbolId name, Pattern* pat) {
    if (pat == null) return false;
    switch (pat.tag) {
        case PAT_CONS:
            return self.is_captured_by_nested_lambda_pattern(name, pat.car_pat)
                || self.is_captured_by_nested_lambda_pattern(name, pat.cdr_pat);
        case PAT_CONSTRUCTOR:
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                if (self.is_captured_by_nested_lambda_pattern(name, pat.ctor_sub_patterns[i])) return true;
            }
            return false;
        case PAT_SEQ:
            for (usz i = 0; i < pat.elem_count; i++) {
                if (self.is_captured_by_nested_lambda_pattern(name, pat.elements[i])) return true;
            }
            return false;
        case PAT_AS:
            return self.is_captured_by_nested_lambda_pattern(name, pat.as_sub);
        case PAT_GUARD:
            if (pattern_binds_name(pat.guard_sub, name)) {
                return self.is_captured_by_nested_lambda_pattern(name, pat.guard_sub);
            }
            if (self.is_captured_by_nested_lambda(name, pat.guard_pred)) return true;
            return self.is_captured_by_nested_lambda_pattern(name, pat.guard_sub);
        case PAT_VAR:
        case PAT_LIT:
        case PAT_QUOTE:
        case PAT_DICT:
        case PAT_WILDCARD:
            return false;
    }
}

fn bool Compiler.is_captured_by_nested_lambda(Compiler* self, SymbolId name, Expr* expr) {
    if (expr == null) return false;
    switch (expr.tag) {
        case E_LAMBDA:
            // Check if this lambda captures name as a free variable
            List{SymbolId} bound;
            if (expr.lambda.param_count > 1) {
                for (usz pi = 0; pi < expr.lambda.param_count; pi++) {
                    bound.push(expr.lambda.params[pi]);
                }
            } else if (expr.lambda.param != INVALID_SYMBOL_ID) {
                bound.push(expr.lambda.param);
            }
            if (expr.lambda.has_rest && expr.lambda.rest_param != INVALID_SYMBOL_ID) {
                bound.push(expr.lambda.rest_param);
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
        case E_DEFINE:
            return self.is_captured_by_nested_lambda(name, expr.define.value);
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
                if (self.is_captured_by_nested_lambda_pattern(name, expr.match.clauses[i].pattern)) return true;
            }
            return false;
        case E_SET:
            return self.is_captured_by_nested_lambda(name, expr.set_expr.value);
        case E_LIT:
        case E_VAR:
        case E_QUOTE:
        case E_RESET:
        case E_SHIFT:
            return false;
        case E_PERFORM:
            if (expr.perform.has_tag_expr && self.is_captured_by_nested_lambda(name, expr.perform.tag_expr)) return true;
            return self.is_captured_by_nested_lambda(name, expr.perform.arg);
        case E_RESOLVE:
            return self.is_captured_by_nested_lambda(name, expr.resolve.value);
        case E_HANDLE:
            if (self.is_captured_by_nested_lambda(name, expr.handle.body)) return true;
            for (usz i = 0; i < expr.handle.clause_count; i++) {
                EffectClause* clause = &expr.handle.clauses[i];
                if (clause.has_effect_tag_expr && self.is_captured_by_nested_lambda(name, clause.effect_tag_expr)) return true;
                if ((uint)clause.k_name == (uint)name || (uint)clause.arg_name == (uint)name) continue;
                if (self.is_captured_by_nested_lambda(name, clause.handler_body)) return true;
            }
            return false;
        case E_INDEX:
            return self.is_captured_by_nested_lambda(name, expr.index.collection) ||
                   self.is_captured_by_nested_lambda(name, expr.index.index);
        case E_WITH_MODULE:
            if (expr.with_module == null) return false;
            for (usz i = 0; i < expr.with_module.body_count; i++) {
                if (self.is_captured_by_nested_lambda(name, expr.with_module.body[i])) return true;
            }
            return false;
        case E_PATH:
        case E_QUASIQUOTE:
        case E_UNQUOTE:
        case E_UNQUOTE_SPLICING:
        case E_DEFMACRO:
        case E_MODULE:
        case E_IMPORT:
        case E_DEFTYPE:
        case E_DEFABSTRACT:
        case E_DEFUNION:
        case E_DEFALIAS:
        case E_DEFEFFECT:
        case E_EXPORT_FROM:
        case E_FFI_LIB:
        case E_FFI_FN:
            return false;
    }
}
