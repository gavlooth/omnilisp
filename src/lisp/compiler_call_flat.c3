module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (call/app forms)
// =============================================================================
// --- Flat (statement-level) compilation of individual forms ---

fn bool Compiler.call_head_is(Compiler* self, Expr* expr, char[] name) {
    if (expr.call.func.tag != E_VAR) return false;
    char[] fname = self.interp.symbols.get_name(expr.call.func.var_expr.name);
    return self.str_eq(fname, name);
}

fn bool Compiler.call_is_explain_thunked(Compiler* self, Expr* expr) {
    if (!self.call_head_is(expr, "explain")) return false;
    if (expr.call.arg_count != 2) return false;

    Expr* thunk = expr.call.args[1];
    if (thunk == null || thunk.tag != E_LAMBDA || thunk.lambda == null) return false;
    if (thunk.lambda.param_count != 0 || thunk.lambda.has_rest) return false;
    return true;
}

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

    if ((uint)selector_sym == (uint)self.interp.sym_effect && target != null && target.tag == E_PERFORM) {
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

    if ((uint)selector_sym == (uint)self.interp.sym_effect
        && target != null
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

    if ((uint)selector_sym == (uint)self.interp.sym_effect && target != null && target.tag == E_RESOLVE) {
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

    if ((uint)selector_sym == (uint)self.interp.sym_effect
        && target != null
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

fn usz Compiler.compile_call_flat(Compiler* self, Expr* expr) {
    usz arg_count = expr.call.arg_count;

    if (self.call_is_explain_thunked(expr)) {
        usz explain_r = self.compile_explain_flat(expr);
        if (explain_r != usz.max) return explain_r;
    }

    if (self.call_head_is(expr, "dict")) return self.compile_list_or_dict_call_flat(expr, true);
    // Keep single-arg list calls on primitive dispatch so runtime conversion
    // semantics (`(list iterator)` / `(list array)`) remain intact.
    if (self.call_head_is(expr, "list") && arg_count != 1) return self.compile_list_or_dict_call_flat(expr, false);

    if (arg_count == 0) {
        usz func_r = self.compile_to_temp(expr.call.func);
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::apply_multi(");
        self.emit_temp_ref(func_r);
        self.emit(", aot::make_nil(), 0);\n");
        return id;
    }

    usz[] arg_temps = self.compile_call_arg_temps(expr.call.args, arg_count);
    defer mem::free(arg_temps.ptr);

    usz func_r = self.compile_to_temp(expr.call.func);
    usz args_list = self.build_arg_list_from_temps(arg_temps, arg_count);

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::apply_multi(");
    self.emit_temp_ref(func_r);
    self.emit(", ");
    self.emit_temp_ref(args_list);
    self.emit(", ");
    self.emit_usz(arg_count);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_call_tail_flat(Compiler* self, Expr* expr) {
    usz arg_count = expr.call.arg_count;

    if (self.call_is_explain_thunked(expr)) {
        usz explain_r = self.compile_explain_flat(expr);
        if (explain_r != usz.max) return explain_r;
    }

    // Special cases: list/dict build data structures, delegate to non-tail version
    if (self.tail_call_requires_non_tail_path(expr)) return self.compile_call_flat(expr);

    if (arg_count == 0) return self.compile_zero_arg_tail_call(expr);

    usz[] arg_temps = self.compile_call_arg_temps(expr.call.args, arg_count);
    defer mem::free(arg_temps.ptr);

    usz func_r = self.compile_to_temp(expr.call.func);
    usz args_list = self.build_arg_list_from_temps(arg_temps, arg_count);

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::apply_multi_tail(");
    self.emit_temp_ref(func_r);
    self.emit(", ");
    self.emit_temp_ref(args_list);
    self.emit(", ");
    self.emit_usz(arg_count);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_app_flat(Compiler* self, Expr* expr) {
    usz func_r = self.compile_to_temp(expr.app.func);
    usz arg_r = self.compile_to_temp(expr.app.arg);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::invoke(");
    self.emit_temp_ref(func_r);
    self.emit(", ");
    self.emit_temp_ref(arg_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_app_tail_flat(Compiler* self, Expr* expr) {
    usz func_r = self.compile_to_temp(expr.app.func);
    usz arg_r = self.compile_to_temp(expr.app.arg);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::invoke_tail(");
    self.emit_temp_ref(func_r);
    self.emit(", ");
    self.emit_temp_ref(arg_r);
    self.emit(");\n");
    return id;
}
