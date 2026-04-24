module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (call arg/list helpers)
// =============================================================================
fn bool Compiler.compile_call_arg_temps(Compiler* self, Expr** args, usz arg_count, usz[]* out_arg_temps) {
    *out_arg_temps = {};
    if (arg_count == 0) return true;

    usz[] arg_temps = {};
    if (!g_compiler_force_call_arg_temps_alloc_fail) {
        arg_temps = mem::new_array(usz, arg_count);
    }
    if (arg_temps.ptr == null) {
        self.set_compile_error("compiler: out of memory while allocating call argument temporaries");
        return false;
    }

    for (usz i = 0; i < arg_count; i++) {
        usz arg_temp = self.compile_to_temp(args[i]);
        if (self.has_error || arg_temp == usz.max) {
            mem::free(arg_temps.ptr);
            self.set_compile_error("compiler: failed to lower call argument");
            return false;
        }
        arg_temps[i] = arg_temp;
    }
    *out_arg_temps = arg_temps;
    return true;
}

fn usz Compiler.build_arg_list_from_temps(Compiler* self, usz[] arg_temps, usz arg_count) {
    if (arg_count > 0 && arg_temps.ptr == null) {
        self.set_compile_error("compiler: missing call argument temporaries");
        return usz.max;
    }

    usz args_list = self.next_result();
    self.emit_temp_decl(args_list);
    self.emit(" = aot::make_nil();\n");
    for (isz i = (isz)arg_count - 1; i >= 0; i--) {
        self.emit_indent();
        self.emit_temp_ref(args_list);
        self.emit(" = aot::cons(");
        self.emit_temp_ref(arg_temps[(usz)i]);
        self.emit(", ");
        self.emit_temp_ref(args_list);
        self.emit(");\n");
    }
    return args_list;
}

fn usz Compiler.compile_list_or_dict_call_flat(Compiler* self, Expr* expr, bool as_dict) {
    usz arg_count = expr.call.arg_count;
    if (arg_count == 0) {
        if (as_dict) {
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::dict_from_args(aot::make_nil());\n");
            return id;
        }
        return self.emit_nil_temp();
    }

    usz[] arg_temps = {};
    if (!self.compile_call_arg_temps(expr.call.args, arg_count, &arg_temps)) return usz.max;
    defer mem::free(arg_temps.ptr);
    usz args_list = self.build_arg_list_from_temps(arg_temps, arg_count);
    if (args_list == usz.max) return usz.max;

    if (as_dict) {
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::dict_from_args(");
        self.emit_temp_ref(args_list);
        self.emit(");\n");
        return id;
    }
    return args_list;
}

fn bool Compiler.tail_call_requires_non_tail_path(Compiler* self, Expr* expr) {
    return false;
}

fn usz Compiler.compile_zero_arg_tail_call(Compiler* self, Expr* expr) {
    usz func_r = self.compile_to_temp(expr.call.func);
    if (self.has_error || func_r == usz.max) return usz.max;
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::apply_multi_tail(");
    self.emit_temp_ref(func_r);
    self.emit(", aot::make_nil(), 0);\n");
    return id;
}
