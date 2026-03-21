module lisp;

import std::collections::list;
import main;
// =============================================================================
// SECTION 4: FREE VARIABLE ANALYSIS (dispatch + qq traversal)
// =============================================================================

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
