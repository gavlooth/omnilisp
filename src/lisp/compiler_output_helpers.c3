module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 3: OUTPUT HELPERS
// =============================================================================

fn void Compiler.emit(Compiler* self, char[] s) {
    self.output.append_string((String)s);
}

fn void Compiler.emit_char(Compiler* self, char c) {
    self.output.append_char(c);
}

/**
 * Emit a string into the output with escaping for C3 string literal context.
 * Replaces \ with \\ and " with \" to prevent code injection via symbol names.
 */
fn void Compiler.emit_escaped(Compiler* self, char[] s) {
    foreach (c : s) {
        if (c == '"') {
            self.emit("\\\"");
        } else if (c == '\\') {
            self.emit("\\\\");
        } else if (c == '\n') {
            self.emit("\\n");
        } else if (c == '\t') {
            self.emit("\\t");
        } else {
            self.emit_char(c);
        }
    }
}

fn void Compiler.emit_line(Compiler* self, char[] s) {
    self.emit_indent();
    self.emit(s);
    self.emit_char('\n');
}

fn void Compiler.emit_indent(Compiler* self) {
    for (usz i = 0; i < self.indent; i++) {
        self.emit("    ");
    }
}

fn void Compiler.emit_newline(Compiler* self) {
    self.emit_char('\n');
}

fn void Compiler.emit_int(Compiler* self, long n) {
    // Convert integer to string manually
    if (n == 0) {
        self.emit_char('0');
        return;
    }

    bool negative = n < 0;
    if (negative) {
        self.emit_char('-');
        n = -n;
    }

    char[32] buf;
    usz pos = 0;
    while (n > 0 && pos < 31) {
        buf[pos++] = (char)('0' + (n % 10));
        n /= 10;
    }

    // Reverse
    for (isz i = (isz)pos - 1; i >= 0; i--) {
        self.emit_char(buf[(usz)i]);
    }
}

fn void Compiler.emit_usz(Compiler* self, usz n) {
    self.emit_int((long)n);
}

fn bool is_c3_reserved(char[] name) {
    char[][*] reserved = {"try", "var", "fn", "if", "else", "for", "while", "do",
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

fn char[] Compiler.get_output(Compiler* self) {
    return self.output.str_view();
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
