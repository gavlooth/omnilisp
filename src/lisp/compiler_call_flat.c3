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

fn usz Compiler.compile_call_flat(Compiler* self, Expr* expr) {
    usz arg_count = expr.call.arg_count;

    if (self.call_is_explain_thunked(expr)) {
        return self.compile_eval_serialized_expr_flat(expr);
    }

    if (self.call_head_is(expr, "dict")) return self.compile_list_or_dict_call_flat(expr, true);
    if (self.call_head_is(expr, "list")) return self.compile_list_or_dict_call_flat(expr, false);

    if (arg_count == 0) {
        usz func_r = self.compile_to_temp(expr.call.func);
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::invoke(");
        self.emit_temp_ref(func_r);
        self.emit(", aot::make_nil());\n");
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
        return self.compile_eval_serialized_expr_flat(expr);
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
