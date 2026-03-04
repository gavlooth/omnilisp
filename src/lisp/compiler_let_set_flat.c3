module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (let/set and primitive checks)
// =============================================================================
fn usz Compiler.compile_if_flat(Compiler* self, Expr* expr) {
    return self.compile_if_common(expr, false);
}

fn void Compiler.emit_let_forward_decl(Compiler* self, SymbolId name) {
    self.emit_indent();
    self.emit("lisp::Value* ");
    self.emit_symbol_name(name);
    self.emit(";\n");
    self.mark_declared(name);
}

fn void Compiler.emit_let_assignment(Compiler* self, SymbolId name, usz init_r, bool with_decl) {
    self.emit_indent();
    if (with_decl) {
        self.emit("lisp::Value* ");
        self.mark_declared(name);
    }
    self.emit_symbol_name(name);
    self.emit(" = ");
    self.emit_temp_ref(init_r);
    self.emit(";\n");
}

fn void Compiler.emit_recursive_closure_self_patch(Compiler* self, Expr* rec_init, SymbolId let_name) {
    if (rec_init.tag != E_LAMBDA) return;
    foreach (&def : self.lambda_defs) {
        if (def.body == rec_init.lambda.body && (uint)def.param == (uint)rec_init.lambda.param) {
            for (usz ci = 0; ci < def.capture_count; ci++) {
                if ((uint)def.captures[ci] == (uint)let_name) {
                    self.emit_indent();
                    self.emit("lisp::aot::AotClosureData* _acd_");
                    self.emit_usz(def.id);
                    self.emit(" = (lisp::aot::AotClosureData*)");
                    self.emit_symbol_name(let_name);
                    self.emit(".prim_val.user_data;\n");
                    self.emit_indent();
                    self.emit("Lambda_");
                    self.emit_usz(def.id);
                    self.emit("* _sp_");
                    self.emit_usz(def.id);
                    self.emit(" = (Lambda_");
                    self.emit_usz(def.id);
                    self.emit("*)_acd_");
                    self.emit_usz(def.id);
                    self.emit(".data;\n");
                    self.emit_indent();
                    self.emit("_sp_");
                    self.emit_usz(def.id);
                    self.emit(".captured_");
                    self.emit_symbol_name(let_name);
                    self.emit(" = ");
                    self.emit_symbol_name(let_name);
                    self.emit(";\n");
                    break;
                }
            }
            break;
        }
    }
}

fn bool Compiler.compile_mutable_captured_let(Compiler* self, Expr* expr, bool tail, usz* result) {
    if (!self.is_mutable_captured_var(expr.let_expr.name)) return false;
    usz init_r = self.compile_to_temp(expr.let_expr.init);
    self.emit_indent();
    self.emit("aot::define_var(\"");
    char[] mc_name = self.interp.symbols.get_name(expr.let_expr.name);
    self.emit_escaped(mc_name);
    self.emit("\", ");
    self.emit_temp_ref(init_r);
    self.emit(");\n");
    if (tail) {
        *result = self.compile_to_temp_tail(expr.let_expr.body);
    } else {
        *result = self.compile_to_temp(expr.let_expr.body);
    }
    return true;
}

fn usz Compiler.compile_let_flat_common(Compiler* self, Expr* expr, bool tail) {
    usz result;
    if (self.compile_mutable_captured_let(expr, tail, &result)) {
        return result;
    }

    bool already = self.is_declared(expr.let_expr.name);
    if (expr.let_expr.is_recursive) {
        if (!already) self.emit_let_forward_decl(expr.let_expr.name);
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_let_assignment(expr.let_expr.name, init_r, false);
        self.emit_recursive_closure_self_patch(expr.let_expr.init, expr.let_expr.name);
    } else {
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_let_assignment(expr.let_expr.name, init_r, !already);
    }

    if (tail) {
        return self.compile_to_temp_tail(expr.let_expr.body);
    }
    return self.compile_to_temp(expr.let_expr.body);
}

fn usz Compiler.compile_let_flat(Compiler* self, Expr* expr) {
    return self.compile_let_flat_common(expr, false);
}

fn usz Compiler.compile_let_flat_tail(Compiler* self, Expr* expr) {
    return self.compile_let_flat_common(expr, true);
}

fn usz Compiler.compile_and_flat(Compiler* self, Expr* expr) {
    usz left = self.compile_to_temp(expr.and_expr.left);
    return self.compile_short_circuit(left, expr.and_expr.right, false, false);
}

fn usz Compiler.compile_or_flat(Compiler* self, Expr* expr) {
    usz left = self.compile_to_temp(expr.or_expr.left);
    return self.compile_short_circuit(left, expr.or_expr.right, true, false);
}

fn usz Compiler.compile_begin_flat(Compiler* self, Expr* expr) {
    return self.compile_begin_common(expr, false);
}

fn usz Compiler.compile_mutable_captured_set_flat(Compiler* self, SymbolId name, Expr* value) {
    usz val_r = self.compile_to_temp(value);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::set_var(\"");
    char[] set_name = self.interp.symbols.get_name(name);
    self.emit_escaped(set_name);
    self.emit("\", ");
    self.emit_temp_ref(val_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_local_set_flat(Compiler* self, SymbolId name, Expr* value) {
    usz val_r = self.compile_to_temp(value);
    self.emit_indent();
    self.emit_symbol_name(name);
    self.emit(" = ");
    self.emit_temp_ref(val_r);
    self.emit(";\n");

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = ");
    self.emit_symbol_name(name);
    self.emit(";\n");
    return id;
}

fn usz Compiler.compile_set_flat(Compiler* self, Expr* expr) {
    if (self.is_mutable_captured_var(expr.set_expr.name)) {
        return self.compile_mutable_captured_set_flat(expr.set_expr.name, expr.set_expr.value);
    }
    return self.compile_local_set_flat(expr.set_expr.name, expr.set_expr.value);
}

fn bool Compiler.is_builtin_primitive(Compiler* self, SymbolId sym) {
    // Check ONLY the hardcoded primitives list, NOT defined_globals.
    // Used for delegation injection — defined globals still need injection
    // into the interpreter env since they're C3 locals, not interpreter vars.
    char[] name = self.interp.symbols.get_name(sym);
    char[][*] primitives = {
        "+", "-", "*", "/", "%",
        "=", "<", ">", "<=", ">=",
        "cons", "car", "cdr", "null?", "pair?",
        "list", "length", "not",
        "print", "println", "newline", "display",
        "string-append", "string-join", "substring",
        "string-split", "string-length", "string->list",
        "list->string", "string-upcase", "string-downcase",
        "string-trim", "string?", "int?", "symbol?",
        "closure?", "continuation?", "double?", "list?",
        "boolean?", "number?",
        "string->number", "number->string",
        "gensym", "load", "apply", "equal?",
        "dict", "dict-set!", "dict?",
        "ref", "push!", "keys", "values", "has?", "remove!",
        "read-file", "write-file", "file-exists?", "read-lines",
        "type-of",
        "abs", "min", "max", "floor", "ceiling", "round",
        "truncate", "sqrt", "even?", "odd?", "zero?",
        "positive?", "negative?", "gcd",
        "bitwise-and", "bitwise-or", "bitwise-xor",
        "bitwise-not", "lshift", "rshift",
        "true", "false", "nil"
    };
    foreach (prim : primitives) {
        if (self.str_eq(name, prim)) return true;
    }
    return false;
}
