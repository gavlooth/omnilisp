module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 4: FREE VARIABLE ANALYSIS (utilities)
// =============================================================================
fn void Compiler.collect_pattern_bindings(Compiler* self, Pattern* pat, List{SymbolId}* bindings) {
    if (pat == null) return;

    switch (pat.tag) {
        case PAT_VAR:
            bindings.push(pat.var_name);

        case PAT_CONS:
            self.collect_pattern_bindings(pat.car_pat, bindings);
            self.collect_pattern_bindings(pat.cdr_pat, bindings);

        case PAT_CONSTRUCTOR:
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                self.collect_pattern_bindings(pat.ctor_sub_patterns[i], bindings);
            }

        case PAT_SEQ:
            for (usz i = 0; i < pat.elem_count; i++) {
                self.collect_pattern_bindings(pat.elements[i], bindings);
            }
            if (pat.rest_pos == REST_MIDDLE) {
                bindings.push(pat.rest_binding);
            }

        case PAT_AS:
            self.collect_pattern_bindings(pat.as_sub, bindings);
            bindings.push(pat.as_name);

        case PAT_GUARD:
            if (pat.guard_sub != null) {
                self.collect_pattern_bindings(pat.guard_sub, bindings);
            }

        case PAT_DICT:
            for (usz i = 0; i < pat.dict_key_count; i++) {
                bindings.push(pat.dict_keys[i]);
            }

        default:
            return;
    }
}

fn bool Compiler.is_primitive(Compiler* self, SymbolId sym) {
    char[] name = self.interp.symbols.get_name(sym);

    // Check known primitives
    char[][*] primitives = {
        "+", "-", "*", "/", "%",
        "=", "<", ">", "<=", ">=",
        "cons", "car", "cdr", "null?", "pair?",
        "List", "list", "length", "not",
        "print", "println", "newline", "display",
        "__ui-ftxui-run",
        "string-append", "string-join", "substring",
        "string-split", "string-length", "string->list",
        "list->string", "string-upcase", "string-downcase", "string-casefold", "string-titlecase",
        "string-trim", "string-contains?",
        "string?", "integer?", "int?", "symbol?",
        "closure?", "continuation?", "double?", "list?",
        "boolean?", "bool?", "number?",
        "string->number", "number->string",
        "Integer", "Double", "String", "Symbol", "Pointer", "Boolean", "Nil", "Void", "Closure", "Iterator",
        "gensym", "load", "apply", "equal?",
        "Array",
        "Dictionary", "dict?",
        "Set", "set-add", "set-remove", "set-contains?", "set-size", "set->list",
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

    // Check if it's a defined global
    foreach (g : self.defined_globals) {
        if ((uint)g == (uint)sym) return true;
    }

    return false;
}

fn bool Compiler.is_mutable_captured_var(Compiler* self, SymbolId sym) {
    foreach (mc : self.mutable_captures) {
        if ((uint)mc == (uint)sym) return true;
    }
    return false;
}

fn bool Compiler.str_eq(Compiler* self, char[] a, char[] b) {
    if (a.len != b.len) return false;
    for (usz i = 0; i < a.len; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}
