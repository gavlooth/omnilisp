module lisp;

import std::collections::list;

const usz COMPILER_SYMBOL_NAME_MAX_EMITTED = 96;
const usz COMPILER_SYMBOL_NAME_HASH_HEX_LEN = 16;
const char[] COMPILER_SYMBOL_NAME_LONG_MARKER = "_omni_sym_";
const char[] COMPILER_SYMBOL_NAME_RESERVED_PREFIX = "_omni_";

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

fn bool compiler_symbol_name_requires_generated_namespace(char[] name) {
    if (name.len > 0) {
        char lead = name[0];
        if ((lead >= 'A' && lead <= 'Z') || (lead >= '0' && lead <= '9')) {
            return true;
        }
    }
    return is_c3_reserved(name);
}

fn bool compiler_symbol_char_is_raw(char c) @inline {
    return (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9') ||
        c == '_';
}

fn bool compiler_symbol_name_needs_stable_suffix(char[] name) {
    if (compiler_symbol_name_requires_generated_namespace(name)) return true;
    if (name.len >= COMPILER_SYMBOL_NAME_RESERVED_PREFIX.len) {
        bool reserved_prefix = true;
        for (usz i = 0; i < COMPILER_SYMBOL_NAME_RESERVED_PREFIX.len; i++) {
            if (name[i] != COMPILER_SYMBOL_NAME_RESERVED_PREFIX[i]) {
                reserved_prefix = false;
                break;
            }
        }
        if (reserved_prefix) return true;
    }
    foreach (c : name) {
        if (!compiler_symbol_char_is_raw(c)) return true;
    }
    return false;
}

fn usz compiler_symbol_sanitized_token_len(char c) @inline {
    switch (c) {
        case '-': return 1;
        case '?': return 2;
        case '!': return 5;
        case '>': return 4;
        case '<': return 4;
        case '=': return 4;
        case '+': return 6;
        case '*': return 6;
        case '/': return 7;
        case '%': return 5;
        default: return compiler_symbol_char_is_raw(c) ? 1 : 5;
    }
}

fn usz compiler_symbol_sanitized_len(char[] name) {
    usz len = 0;
    foreach (c : name) len += compiler_symbol_sanitized_token_len(c);
    return len;
}

fn ulong compiler_symbol_name_hash(char[] name) {
    ulong h = (ulong)1469598103934665603;
    foreach (c : name) {
        h = (h ^ (ulong)((uint)c & 0xFF)) * (ulong)1099511628211;
    }
    return h ^ (ulong)name.len;
}

fn void Compiler.emit_symbol_hash_hex(Compiler* self, ulong hash) {
    char[] hex = "0123456789abcdef";
    for (usz i = 0; i < COMPILER_SYMBOL_NAME_HASH_HEX_LEN; i++) {
        usz shift = (COMPILER_SYMBOL_NAME_HASH_HEX_LEN - 1 - i) * 4;
        self.emit_char(hex[(usz)((hash >> shift) & (ulong)0xF)]);
    }
}

fn usz compiler_symbol_usz_decimal_len(usz n) {
    usz len = 1;
    while (n >= 10) {
        n /= 10;
        len++;
    }
    return len;
}

fn void Compiler.emit_symbol_sanitized_token(Compiler* self, char c) {
    char[] hex = "0123456789abcdef";
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
    } else if (!compiler_symbol_char_is_raw(c)) {
        uint byte = (uint)c & 0xFF;
        self.emit("_x");
        self.emit_char(hex[(usz)((byte >> 4) & 0xF)]);
        self.emit_char(hex[(usz)(byte & 0xF)]);
        self.emit_char('_');
    } else {
        self.emit_char(c);
    }
}

fn void Compiler.emit_symbol_sanitized_token_limited(Compiler* self, char[] token, usz* emitted, usz limit) {
    for (usz i = 0; i < token.len && *emitted < limit; i++) {
        self.emit_char(token[i]);
        (*emitted)++;
    }
}

fn void Compiler.emit_symbol_sanitized_prefix(Compiler* self, char[] name, usz limit) {
    char[] hex = "0123456789abcdef";
    usz emitted = 0;
    foreach (c : name) {
        if (c == '-') {
            self.emit_symbol_sanitized_token_limited("_", &emitted, limit);
        } else if (c == '?') {
            self.emit_symbol_sanitized_token_limited("_p", &emitted, limit);
        } else if (c == '!') {
            self.emit_symbol_sanitized_token_limited("_bang", &emitted, limit);
        } else if (c == '>') {
            self.emit_symbol_sanitized_token_limited("_to_", &emitted, limit);
        } else if (c == '<') {
            self.emit_symbol_sanitized_token_limited("_lt_", &emitted, limit);
        } else if (c == '=') {
            self.emit_symbol_sanitized_token_limited("_eq_", &emitted, limit);
        } else if (c == '+') {
            self.emit_symbol_sanitized_token_limited("_plus_", &emitted, limit);
        } else if (c == '*') {
            self.emit_symbol_sanitized_token_limited("_star_", &emitted, limit);
        } else if (c == '/') {
            self.emit_symbol_sanitized_token_limited("_slash_", &emitted, limit);
        } else if (c == '%') {
            self.emit_symbol_sanitized_token_limited("_mod_", &emitted, limit);
        } else if (!compiler_symbol_char_is_raw(c)) {
            uint byte = (uint)c & 0xFF;
            char[5] escaped = { '_', 'x', hex[(usz)((byte >> 4) & 0xF)], hex[(usz)(byte & 0xF)], '_' };
            self.emit_symbol_sanitized_token_limited(escaped[..], &emitted, limit);
        } else {
            if (emitted >= limit) return;
            self.emit_char(c);
            emitted++;
        }
        if (emitted >= limit) return;
    }
}

fn void Compiler.emit_symbol_sanitized_full(Compiler* self, char[] name) {
    foreach (c : name) {
        self.emit_symbol_sanitized_token(c);
    }
}

fn void Compiler.emit_symbol_name(Compiler* self, SymbolId sym) {
    char[] name = self.interp.symbols.get_name(sym);
    usz sanitized_len = compiler_symbol_sanitized_len(name);
    bool needs_stable_suffix = compiler_symbol_name_needs_stable_suffix(name) ||
        sanitized_len > COMPILER_SYMBOL_NAME_MAX_EMITTED;

    if (!needs_stable_suffix) {
        self.emit_symbol_sanitized_full(name);
        return;
    }

    char[] hash_marker = "_h";
    char[] len_marker = "_l";
    usz suffix_len = COMPILER_SYMBOL_NAME_LONG_MARKER.len +
        hash_marker.len +
        COMPILER_SYMBOL_NAME_HASH_HEX_LEN +
        len_marker.len +
        compiler_symbol_usz_decimal_len(name.len);
    usz prefix_limit = COMPILER_SYMBOL_NAME_MAX_EMITTED > suffix_len
        ? COMPILER_SYMBOL_NAME_MAX_EMITTED - suffix_len
        : 0;
    self.emit(COMPILER_SYMBOL_NAME_LONG_MARKER);
    self.emit_symbol_sanitized_prefix(name, prefix_limit);
    self.emit(hash_marker);
    self.emit_symbol_hash_hex(compiler_symbol_name_hash(name));
    self.emit(len_marker);
    self.emit_usz(name.len);
}

fn bool compiler_prim_emit_code_is_runtime_lookup(char[] emit_code) @inline {
    return emit_code.len > 18 && emit_code[0] == 'a' && emit_code[4] == ':';
}

fn bool Compiler.symbol_is_active_primitive_shadow(Compiler* self, SymbolId sym) {
    for (usz i = self.primitive_shadow_symbols.len(); i > 0; i--) {
        if ((uint)self.primitive_shadow_symbols.entries[i - 1] == (uint)sym) return true;
    }
    return false;
}

fn bool Compiler.push_primitive_shadow_if_needed(Compiler* self, SymbolId sym) {
    if (prim_hash_lookup(sym).len == 0) return false;
    self.primitive_shadow_symbols.push(sym);
    return true;
}

fn void Compiler.pop_primitive_shadow(Compiler* self, bool pushed) {
    if (!pushed || self.primitive_shadow_symbols.len() == 0) return;
    (void)self.primitive_shadow_symbols.pop();
}

fn void Compiler.emit_symbol_value_reference(Compiler* self, SymbolId sym) {
    SymbolId lexical_backing = INVALID_SYMBOL_ID;
    if (self.find_active_lexical_alias_from(sym, self.module_binding_lexical_alias_mark, &lexical_backing)) {
        self.emit_symbol_name(lexical_backing);
        return;
    }

    SymbolId mutable_cell = INVALID_SYMBOL_ID;
    if (self.find_active_mutable_cell_from(sym, self.module_binding_mutable_cell_mark, &mutable_cell)) {
        self.emit("aot::mutable_cell_get(");
        self.emit_symbol_name(mutable_cell);
        self.emit(")");
        return;
    }

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

    if (self.find_active_lexical_alias(sym, &lexical_backing)) {
        self.emit_symbol_name(lexical_backing);
        return;
    }

    if (self.find_active_mutable_cell(sym, &mutable_cell)) {
        self.emit("aot::mutable_cell_get(");
        self.emit_symbol_name(mutable_cell);
        self.emit(")");
        return;
    }

    if (self.is_global(sym) || self.symbol_is_active_primitive_shadow(sym)) {
        self.emit_symbol_name(sym);
        return;
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

    if (self.is_declared(sym)) {
        self.emit_symbol_name(sym);
        return;
    }

    char[] sym_name = self.interp.symbols.get_name(sym);
    self.emit("aot::lookup_var(\"");
    self.emit_escaped(sym_name);
    self.emit("\")");
}

fn bool Compiler.emit_symbol_assignment_target(Compiler* self, SymbolId sym) {
    SymbolId backing = INVALID_SYMBOL_ID;
    if (self.find_active_lexical_alias_from(sym, self.module_binding_lexical_alias_mark, &backing)) {
        self.emit_symbol_name(backing);
        return true;
    }
    SymbolId mutable_cell = INVALID_SYMBOL_ID;
    if (self.find_active_mutable_cell_from(sym, self.module_binding_mutable_cell_mark, &mutable_cell)) {
        self.emit_symbol_name(mutable_cell);
        return true;
    }
    if (self.find_active_module_private_backing(sym, &backing)) {
        self.emit_symbol_name(backing);
        return true;
    }
    for (usz i = self.module_open_exports.len(); i > 0; i--) {
        if ((uint)self.module_open_exports.entries[i - 1] == (uint)sym) {
            self.emit_symbol_name(self.module_open_aliases.entries[i - 1]);
            return true;
        }
    }
    if (self.module_private_symbols.len() > 0) {
        self.set_compile_error("compiler: inline module local assignment missing private backing");
        self.emit_symbol_name(sym);
        return false;
    }
    if (self.find_active_lexical_alias(sym, &backing)) {
        self.emit_symbol_name(backing);
        return true;
    }
    if (self.find_active_mutable_cell(sym, &mutable_cell)) {
        self.emit_symbol_name(mutable_cell);
        return true;
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
