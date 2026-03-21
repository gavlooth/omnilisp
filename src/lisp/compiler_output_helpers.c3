module lisp;

import std::io;
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

fn void Compiler.emit_string_literal(Compiler* self, char[] s) {
    self.emit_char('"');
    self.emit_escaped(s);
    self.emit_char('"');
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

fn char[] Compiler.get_output(Compiler* self) {
    return self.output.str_view();
}
