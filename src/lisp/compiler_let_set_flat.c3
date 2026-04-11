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

fn usz Compiler.compile_and_flat(Compiler* self, Expr* expr) {
    usz left = self.compile_to_temp(expr.and_expr.left);
    return self.compile_short_circuit(left, expr.and_expr.right, false, false);
}

fn usz Compiler.compile_or_flat(Compiler* self, Expr* expr) {
    usz left = self.compile_to_temp(expr.or_expr.left);
    return self.compile_short_circuit(left, expr.or_expr.right, true, false);
}

fn usz Compiler.compile_block_flat(Compiler* self, Expr* expr) {
    return self.compile_block_common(expr, false);
}

fn usz Compiler.compile_mutable_captured_set_flat(Compiler* self, SymbolId name, Expr* value) {
    usz val_r = self.compile_to_temp(value);
    self.emit_indent();
    self.emit("aot::set_var(\"");
    char[] set_name = self.interp.symbols.get_name(name);
    self.emit_escaped(set_name);
    self.emit("\", ");
    self.emit_temp_ref(val_r);
    self.emit(");\n");

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_void();\n");
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
    self.emit(" = aot::make_void();\n");
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
        "List", "list", "length", "not",
        "print", "println", "newline", "display",
        "__ui-ftxui-run",
        "string-append", "string-join", "substring",
        "string-split", "string-length",
        "string-upcase", "string-downcase", "string-casefold", "string-titlecase",
        "string-trim", "string-contains?",
        "string?", "integer?", "int?", "symbol?",
        "closure?", "continuation?", "double?", "list?",
        "boolean?", "bool?", "number?",
        "string->number",
        "Integer", "Double", "String", "Symbol", "Pointer", "Boolean", "Nil", "Void", "Closure", "Iterator",
        "gensym", "load", "apply", "equal?",
        "Array",
        "Dictionary", "dict?",
        "Set", "set-add", "set-remove", "set-contains?",
        "ref", "set!", "push!", "keys", "values", "has?", "remove!",
        "iterator?", "__make-iterator", "next",
        "Coroutine", "resume", "yield", "coroutine?",
        "json-get",
        "TimePoint", "time-point?",
        "read-line", "read-file", "write-file", "file-exists?", "read-lines",
        "fs-open", "fs-read", "fs-write", "fs-close",
        "fs-stat", "fs-readdir", "fs-rename", "fs-unlink",
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
