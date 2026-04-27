module lisp;

import std::io;
import std::collections::list;
import main;

/**
 * Traverse quasiquote body, only descending into unquote/unquote-splicing for free vars.
 * In quasiquote, bare symbols are literals - only unquoted expressions reference variables.
 */
fn void Compiler.find_free_vars_in_qq(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    if (expr == null) return;
    switch (expr.tag) {
        case E_UNQUOTE:
            // Unquoted expression - evaluate normally, so scan for free vars
            self.find_free_vars(expr.unquote.body, bound_vars, free_vars, enclosing_scope);
        case E_UNQUOTE_SPLICING:
            self.find_free_vars(expr.unquote_splicing.body, bound_vars, free_vars, enclosing_scope);
        case E_CALL:
            // Lists in qq template are parsed as E_CALL - traverse to find unquotes
            self.find_free_vars_in_qq(expr.call.func, bound_vars, free_vars, enclosing_scope);
            for (usz i = 0; i < expr.call.arg_count; i++) {
                self.find_free_vars_in_qq(expr.call.args[i], bound_vars, free_vars, enclosing_scope);
            }
        case E_APP:
            self.find_free_vars_in_qq(expr.app.func, bound_vars, free_vars, enclosing_scope);
            self.find_free_vars_in_qq(expr.app.arg, bound_vars, free_vars, enclosing_scope);
        case E_QUASIQUOTE:
            // Nested quasiquote - still only scan for unquotes
            self.find_free_vars_in_qq(expr.quasiquote.body, bound_vars, free_vars, enclosing_scope);
        case E_LIT:
        case E_VAR:
        case E_LAMBDA:
        case E_IF:
        case E_LET:
        case E_DEFINE:
        case E_QUOTE:
        case E_RESET:
        case E_SHIFT:
        case E_PERFORM:
        case E_HANDLE:
        case E_RESOLVE:
        case E_INDEX:
        case E_PATH:
        case E_MATCH:
        case E_AND:
        case E_OR:
        case E_BLOCK:
        case E_SET:
        case E_DEFMACRO:
        case E_MODULE:
        case E_IMPORT:
        case E_WITH_MODULE:
        case E_DEFTYPE:
        case E_DEFABSTRACT:
        case E_DEFUNION:
        case E_DEFALIAS:
        case E_DEFEFFECT:
        case E_EXPORT_FROM:
        case E_FFI_LIB:
        case E_FFI_FN:
            return;
        default:
            self.set_compile_error("compiler: unknown expression tag in quasiquote free-variable analysis");
            return;
    }
}
