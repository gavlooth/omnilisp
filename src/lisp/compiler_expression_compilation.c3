module lisp;

import std::collections::list;
import main;
// =============================================================================
// SECTION 8: EXPRESSION COMPILATION
// =============================================================================

fn void Compiler.compile_expr(Compiler* self, Expr* expr) {
    if (expr == null) {
        self.emit("aot::make_nil()");
        return;
    }

    switch (expr.tag) {
        case E_LIT:
            self.compile_literal(expr.lit.value);

        case E_VAR:
            self.compile_var(expr);

        case E_DEFINE:
            // Define in expression context
            self.emit("(");
            self.emit_symbol_name(expr.define.name);
            self.emit(" = ");
            self.compile_expr(expr.define.value);
            self.emit(")");

        case E_QUOTE:
            self.compile_quote(expr.quote.datum);

        case E_PATH:
            self.compile_path(expr);

        default:
            self.set_unsupported_expr_error(expr.tag);
            self.emit("aot::make_nil()");
    }
}
