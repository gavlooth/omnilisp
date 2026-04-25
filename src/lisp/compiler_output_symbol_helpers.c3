module lisp;

import std::collections::list;

fn bool is_c3_reserved(char[] name) {
    char[][*] reserved = {"try", "var", "fn", "if", "else", "for", "while", "block",
        "switch", "case", "default", "return", "break", "continue", "import",
        "module", "struct", "enum", "union", "const", "static", "true", "false",
        "null", "void", "bool", "int", "uint", "char", "float", "double", "usz"};
    foreach (r : reserved) {
        if (name.len == r.len) {
            bool match = true;
            for (usz i = 0; i < name.len; i++) {
                if (name[i] != r[i]) { match = false; break; }
            }
            if (match) return true;
        }
    }
    return false;
}

fn void Compiler.emit_symbol_name(Compiler* self, SymbolId sym) {
    char[] name = self.interp.symbols.get_name(sym);
    if (name.len > 0) {
        char lead = name[0];
        // C3 rejects global identifiers starting with uppercase; keep Omni
        // symbol spelling intact by applying a stable generated-name prefix.
        if ((lead >= 'A' && lead <= 'Z') || (lead >= '0' && lead <= '9')) {
            self.emit("_omni_v_");
        }
    }
    // Escape C3 reserved words
    if (is_c3_reserved(name)) {
        self.emit("_omni_");
    }
    // Sanitize name for C3 (replace - with _)
    foreach (c : name) {
        if (c == '-') {
            self.emit_char('_');
        } else if (c == '?') {
            self.emit("_p");  // predicate suffix
        } else if (c == '!') {
            self.emit("_bang");
        } else if (c == '>') {
            self.emit("_to_");
        } else if (c == '<') {
            self.emit("_lt_");
        } else if (c == '=') {
            self.emit("_eq_");
        } else if (c == '+') {
            self.emit("_plus_");
        } else if (c == '*') {
            self.emit("_star_");
        } else if (c == '/') {
            self.emit("_slash_");
        } else if (c == '%') {
            self.emit("_mod_");
        } else {
            self.emit_char(c);
        }
    }
}

fn bool compiler_prim_emit_code_is_runtime_lookup(char[] emit_code) @inline {
    return emit_code.len > 18 && emit_code[0] == 'a' && emit_code[4] == ':';
}

fn void Compiler.emit_symbol_value_reference(Compiler* self, SymbolId sym) {
    for (usz i = self.module_private_symbols.len(); i > 0; i--) {
        if ((uint)self.module_private_symbols.entries[i - 1] == (uint)sym) {
            self.emit_symbol_name(self.module_private_backings.entries[i - 1]);
            return;
        }
    }

    for (usz i = self.module_open_exports.len(); i > 0; i--) {
        if ((uint)self.module_open_exports.entries[i - 1] == (uint)sym) {
            self.emit_symbol_name(self.module_open_aliases.entries[i - 1]);
            return;
        }
    }

    char[] emit_code = prim_hash_lookup(sym);
    if (emit_code.len > 0) {
        if (compiler_prim_emit_code_is_runtime_lookup(emit_code)) {
            self.record_prim_ref(sym);
            self.emit_prim_global_name(sym);
        } else {
            self.emit(emit_code);
        }
        return;
    }

    if (self.is_mutable_captured_var(sym)) {
        char[] sym_name = self.interp.symbols.get_name(sym);
        self.emit("aot::lookup_var(\"");
        self.emit_escaped(sym_name);
        self.emit("\")");
        return;
    }

    if (!self.is_global(sym) && !self.is_declared(sym)) {
        char[] sym_name = self.interp.symbols.get_name(sym);
        self.emit("aot::lookup_var(\"");
        self.emit_escaped(sym_name);
        self.emit("\")");
        return;
    }

    self.emit_symbol_name(sym);
}

fn bool Compiler.emit_symbol_assignment_target(Compiler* self, SymbolId sym) {
    SymbolId backing = INVALID_SYMBOL_ID;
    if (self.find_active_module_private_backing(sym, &backing)) {
        self.emit_symbol_name(backing);
        return true;
    }
    if (self.module_private_symbols.len() > 0) {
        self.set_compile_error("compiler: inline module local assignment missing private backing");
        self.emit_symbol_name(sym);
        return false;
    }
    self.emit_symbol_name(sym);
    return true;
}

/**
 * D2: Emit the global variable name for a cached primitive.
 * Uses _prim_ prefix followed by the sanitized symbol name.
 */
fn void Compiler.emit_prim_global_name(Compiler* self, SymbolId sym) {
    self.emit("_prim_");
    self.emit_symbol_name(sym);
}

/**
 * D2: Record a primitive as referenced (for global caching).
 * Only adds if not already present.
 */
fn void Compiler.record_prim_ref(Compiler* self, SymbolId sym) {
    foreach (s : self.referenced_prims) {
        if ((uint)s == (uint)sym) return;
    }
    self.referenced_prims.push(sym);
}
