module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (resolve/index/define forms)
// =============================================================================
fn usz Compiler.compile_resolve_flat(Compiler* self, Expr* expr) {
    usz val_r = self.compile_to_temp(expr.resolve.value);
    Expr dummy_k;
    dummy_k.tag = E_VAR;
    dummy_k.var_expr.name = self.interp.sym__k;
    usz k_r = self.compile_to_temp(&dummy_k);

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::compiled_resolve(");
    self.emit_temp_ref(k_r);
    self.emit(", ");
    self.emit_temp_ref(val_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_index_flat(Compiler* self, Expr* expr) {
    usz coll_r = self.compile_to_temp(expr.index.collection);
    usz idx_r = self.compile_to_temp(expr.index.index);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::index(");
    self.emit_temp_ref(coll_r);
    self.emit(", ");
    self.emit_temp_ref(idx_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_eval_serialized_expr_flat(Compiler* self, Expr* expr) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::eval_serialized_expr(");
    self.emit_serialized_expr(expr);
    self.emit(");\n");
    return id;
}

fn bool Compiler.is_typed_lambda_define(Compiler* self, Expr* expr) @inline {
    if (expr == null || expr.tag != E_DEFINE) return false;
    Expr* value = expr.define.value;
    if (value == null || value.tag != E_LAMBDA || value.lambda == null) return false;
    return value.lambda.has_typed_params;
}

fn usz Compiler.compile_define_rhs_with_typed_bridge(Compiler* self, Expr* define_expr) {
    if (self.is_typed_lambda_define(define_expr)) {
        // Typed defines install/merge runtime dispatch entries by side effect.
        // Use the post-define global binding as the canonical callable value.
        usz eval_r = self.compile_eval_serialized_expr_flat(define_expr);
        (void)eval_r;
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::lookup_var(\"");
        self.emit_escaped(self.interp.symbols.get_name(define_expr.define.name));
        self.emit("\");\n");
        return id;
    }
    return self.compile_to_temp(define_expr.define.value);
}

fn usz Compiler.compile_define_flat(Compiler* self, Expr* expr) {
    usz def_val_r = self.compile_define_rhs_with_typed_bridge(expr);
    self.emit_indent();
    self.emit_symbol_name(expr.define.name);
    self.emit(" = ");
    self.emit_temp_ref(def_val_r);
    self.emit(";\n");
    return def_val_r;
}
