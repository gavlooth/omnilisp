module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 7: STATEMENT COMPILATION
// =============================================================================

fn void Compiler.compile_statement(Compiler* self, Expr* expr) {
    if (expr == null) return;

    switch (expr.tag) {
        case E_DEFINE:
            // (define name value) => name = <compile value>;
            usz init_r = self.compile_to_temp(expr.define.value);
            self.emit_indent();
            self.emit_symbol_assignment_target(expr.define.name);
            self.emit(" = ");
            self.emit_temp_ref(init_r);
            self.emit(";\n");
        case E_FFI_LIB:
            usz ffi_lib_r = self.compile_to_temp(expr);
            self.emit_indent();
            self.emit_symbol_assignment_target(expr.ffi_lib.name);
            self.emit(" = ");
            self.emit_temp_ref(ffi_lib_r);
            self.emit(";\n");
        case E_FFI_FN:
            if (expr.ffi_fn != null) {
                usz ffi_fn_r = self.compile_to_temp(expr);
                self.emit_indent();
                self.emit_symbol_assignment_target(expr.ffi_fn.fn_name);
                self.emit(" = ");
                self.emit_temp_ref(ffi_fn_r);
                self.emit(";\n");
            } else {
                self.compile_to_temp(expr);
            }

        default:
            // Other expressions: evaluate and discard result
            self.compile_to_temp(expr);
    }
}
