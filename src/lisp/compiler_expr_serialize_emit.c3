module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 4c: EXPRESSION SERIALIZER (buffer append + emit)
// =============================================================================
fn void Compiler.buf_append(Compiler* self, List{char}* buf, char[] s) {
    foreach (c : s) {
        buf.push(c);
    }
}

/**
 * Emit the serialized form of an expression as a C3 string literal
 * directly into the compiler output. Escapes special characters.
 */
fn void Compiler.emit_serialized_expr(Compiler* self, Expr* expr) {
    List{char} buf;
    self.serialize_expr_to_buf(expr, &buf);

    // Emit the serialized source as a C3 string literal with escaping
    self.emit("\"");
    for (usz i = 0; i < buf.len(); i++) {
        char c = buf.entries[i];
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
    self.emit("\"");

    buf.free();
}

/**
 * Collect free variables from a continuation expression (reset/shift/handle/perform).
 * These are variables that need to be injected into the interpreter before evaluation.
 * Returns the list of free variables (caller must free).
 */
fn List{SymbolId} Compiler.collect_cont_free_vars(Compiler* self, Expr* expr) {
    List{SymbolId} bound;
    List{SymbolId} free;
    self.for_delegation = true;
    self.find_free_vars(expr, &bound, &free, null);
    self.for_delegation = false;
    bound.free();
    return free;
}
