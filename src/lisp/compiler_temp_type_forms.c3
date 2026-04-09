module lisp;

import std::io;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (type-form lowering)
// =============================================================================
fn usz Compiler.compile_typed_define_direct(Compiler* self, Expr* define_expr) {
    usz value_r = self.compile_to_temp(define_expr.define.value);
    ExprLambda* lambda = define_expr.define.value.lambda;
    usz params_id = self.next_result();
    char[64] params_name_buf;
    char[] params_name = compiler_format_generated_name_id(params_name_buf[..], "_aot_typed_params_", (long)params_id);

    if (lambda.param_count > 0) {
        self.emit_indent();
        self.emit("lisp::aot::AotTypeAnnotationSpec[");
        self.emit_usz(lambda.param_count);
        self.emit("] ");
        self.emit(params_name);
        self.emit(";\n");
        for (usz i = 0; i < lambda.param_count; i++) {
            char[96] target_buf;
            char[] target = compiler_format_generated_name_index(target_buf[..], params_name, (long)i);
            self.emit_aot_type_annotation_spec_init(target, &lambda.param_annotations[i]);
        }
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::define_typed_method(");
    self.emit_string_literal(self.interp.symbols.get_name(define_expr.define.name));
    self.emit(", ");
    self.emit_temp_ref(value_r);
    self.emit(", ");
    if (lambda.param_count > 0) {
        self.emit("&");
        self.emit(params_name);
        self.emit("[0]");
    } else {
        self.emit("null");
    }
    self.emit(", ");
    self.emit_usz(lambda.param_count);
    self.emit(");\n");
    return id;
}
fn usz Compiler.compile_type_form_direct(Compiler* self, Expr* expr) {
    switch (expr.tag) {
        case E_DEFTYPE:
            return self.compile_deftype_direct(expr);
        case E_DEFABSTRACT:
            return self.compile_defabstract_direct(expr);
        case E_DEFUNION:
            return self.compile_defunion_direct(expr);
        case E_DEFALIAS:
            return self.compile_defalias_direct(expr);
        case E_DEFEFFECT:
            return self.compile_defeffect_direct(expr);
        default:
            // Type-form lowering is explicit only; no serialized-eval fallback bridge.
            return self.compile_leaf_expr_to_temp(expr);
    }
}
