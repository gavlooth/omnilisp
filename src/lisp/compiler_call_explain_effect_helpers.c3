module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (call/app explain effect helpers)
// =============================================================================
fn usz Compiler.compile_explain_effect_signal_flat(Compiler* self, Expr* target) {
    if (target != null && target.tag == E_PERFORM && !target.perform.has_tag_expr) {
        usz arg_r = self.compile_to_temp(target.perform.arg);
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::explain_effect_signal(");
        self.emit_serialized_expr(target);
        self.emit(", ");
        self.emit_string_literal(self.interp.symbols.get_name(target.perform.tag));
        self.emit(", ");
        self.emit_temp_ref(arg_r);
        self.emit(");\n");
        return id;
    }

    if (target != null
        && target.tag == E_CALL
        && (self.call_head_is(target, "perform") || self.call_head_is(target, "signal"))
        && target.call.arg_count == 2
        && target.call.args[0] != null
        && target.call.args[0].tag == E_VAR) {
        usz arg_r = self.compile_to_temp(target.call.args[1]);
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::explain_effect_signal(");
        self.emit_serialized_expr(target);
        self.emit(", ");
        self.emit_string_literal(self.interp.symbols.get_name(target.call.args[0].var_expr.name));
        self.emit(", ");
        self.emit_temp_ref(arg_r);
        self.emit(");\n");
        return id;
    }

    return usz.max;
}

fn usz Compiler.compile_explain_effect_resolve_flat(Compiler* self, Expr* target) {
    if (target != null && target.tag == E_RESOLVE) {
        Expr dummy_k;
        dummy_k.tag = E_VAR;
        dummy_k.var_expr.name = self.interp.sym__k;
        usz k_r = self.compile_to_temp(&dummy_k);
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::explain_effect_resolve(");
        self.emit_serialized_expr(target);
        self.emit(", ");
        self.emit_temp_ref(k_r);
        self.emit(");\n");
        return id;
    }

    if (target != null
        && target.tag == E_CALL
        && self.call_head_is(target, "resolve")
        && target.call.arg_count == 1) {
        Expr dummy_k;
        dummy_k.tag = E_VAR;
        dummy_k.var_expr.name = self.interp.sym__k;
        usz k_r = self.compile_to_temp(&dummy_k);
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::explain_effect_resolve(");
        self.emit_serialized_expr(target);
        self.emit(", ");
        self.emit_temp_ref(k_r);
        self.emit(");\n");
        return id;
    }

    return usz.max;
}
