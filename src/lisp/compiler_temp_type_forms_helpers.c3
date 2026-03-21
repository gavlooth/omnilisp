module lisp;

import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (type-form emit helpers)
// =============================================================================
fn void Compiler.emit_bool_literal(Compiler* self, bool value) @inline {
    self.emit(value ? "true" : "false");
}

fn void Compiler.emit_value_tag_literal(Compiler* self, ValueTag tag) {
    switch (tag) {
        case INT:
            self.emit("lisp::ValueTag.INT");
        case SYMBOL:
            self.emit("lisp::ValueTag.SYMBOL");
        case STRING:
            self.emit("lisp::ValueTag.STRING");
        default:
            self.emit("lisp::ValueTag.NIL");
    }
}

fn void Compiler.emit_target_assignment_prefix(Compiler* self, char[] target, char[] field) {
    self.emit_indent();
    self.emit(target);
    self.emit(".");
    self.emit(field);
    self.emit(" = ");
}

fn void Compiler.emit_string_array_init(Compiler* self, char[] name, char[]* values, usz count) {
    if (count == 0) return;
    self.emit_indent();
    self.emit("char[][");
    self.emit_usz(count);
    self.emit("] ");
    self.emit(name);
    self.emit(";\n");
    for (usz i = 0; i < count; i++) {
        self.emit_indent();
        self.emit(name);
        self.emit("[");
        self.emit_usz(i);
        self.emit("] = ");
        self.emit_string_literal(values[i]);
        self.emit(";\n");
    }
}
