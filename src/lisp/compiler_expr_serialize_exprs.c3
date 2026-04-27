module lisp;

import std::collections::list;
import main;
// =============================================================================
// SECTION 4c: EXPRESSION SERIALIZER (expression dispatch)
// =============================================================================
fn void Compiler.serialize_expr_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    if (expr == null) {
        self.buf_append(buf, "nil");
        return;
    }

    switch (expr.tag) {
        case E_LIT:
            self.serialize_value_to_buf(expr.lit.value, buf);

        case E_VAR:
            char[] name = self.interp.symbols.get_name(expr.var_expr.name);
            self.buf_append(buf, name);

        case E_LAMBDA:
            self.serialize_lambda_to_buf(expr, buf);

        case E_LET:
            self.serialize_let_to_buf(expr, buf);

        case E_HANDLE:
            self.serialize_handle_to_buf(expr, buf);

        case E_CALL:
            self.serialize_call_to_buf(expr, buf);

        case E_MATCH:
            self.serialize_match_to_buf(expr, buf);

        case E_DEFMACRO:
            self.serialize_defmacro_to_buf(expr, buf);

        case E_MODULE:
            self.serialize_module_to_buf(expr, buf);

        case E_IMPORT:
            self.serialize_import_to_buf(expr, buf);

        case E_WITH_MODULE:
            self.serialize_with_module_to_buf(expr, buf);

        case E_EXPORT_FROM:
            self.serialize_export_from_to_buf(expr, buf);

        case E_FFI_LIB:
            self.serialize_ffi_lib_to_buf(expr, buf);

        case E_FFI_FN:
            self.serialize_ffi_fn_to_buf(expr, buf);

        default:
            if (self.serialize_expr_specialized(expr, buf)) return;
            self.set_compile_error("compiler: cannot serialize unknown expression tag");
    }
}
