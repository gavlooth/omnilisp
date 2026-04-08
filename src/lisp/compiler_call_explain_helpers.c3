module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (call/app explain helpers)
// =============================================================================
fn usz Compiler.compile_explain_flat(Compiler* self, Expr* expr) {
    Expr* selector = expr.call.args[0];
    Expr* thunk = expr.call.args[1];
    Expr* target = thunk.lambda.body;
    if (selector == null || selector.tag != E_QUOTE || selector.quote.datum == null || selector.quote.datum.tag != SYMBOL) {
        return usz.max;
    }

    SymbolId selector_sym = selector.quote.datum.sym_val;
    SymbolId dispatch_sym = self.interp.symbols.intern("dispatch");

    if ((uint)selector_sym == (uint)dispatch_sym && target != null && target.tag == E_CALL) {
        usz target_arg_count = target.call.arg_count;
        usz func_r = self.compile_to_temp(target.call.func);
        usz args_list = self.emit_nil_temp();
        if (target_arg_count > 0) {
            usz[] arg_temps = self.compile_call_arg_temps(target.call.args, target_arg_count);
            defer mem::free(arg_temps.ptr);
            args_list = self.build_arg_list_from_temps(arg_temps, target_arg_count);
        }

        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::explain_dispatch(");
        self.emit_serialized_expr(target);
        self.emit(", ");
        self.emit_temp_ref(func_r);
        self.emit(", ");
        self.emit_temp_ref(args_list);
        self.emit(", ");
        self.emit_usz(target_arg_count);
        self.emit(");\n");
        return id;
    }

    if ((uint)selector_sym == (uint)self.interp.sym_effect) {
        usz effect_r = self.compile_explain_effect_signal_flat(target);
        if (effect_r != usz.max) return effect_r;
        effect_r = self.compile_explain_effect_resolve_flat(target);
        if (effect_r != usz.max) return effect_r;
    }

    return usz.max;
}
