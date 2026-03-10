module lisp;

import std::io;
import main;

// =============================================================================
// SECTION: NATIVE MATCH/PATTERN COMPILATION (binding helpers)
// =============================================================================
fn void Compiler.emit_pattern_var_decl(Compiler* self, SymbolId name) {
    if (!self.is_declared(name)) {
        self.emit("lisp::Value* ");
        self.mark_declared(name);
    }
}

fn void Compiler.emit_pattern_var_assign(Compiler* self, SymbolId name, char[] rhs) {
    self.emit_pattern_var_decl(name);
    self.emit_symbol_name(name);
    self.emit(" = ");
    self.emit(rhs);
    self.emit("; ");
}

fn void Compiler.emit_pattern_seq_elem_access(Compiler* self, Pattern* pat, char[] val_name, usz idx) {
    self.emit("lisp::Value* _elem_");
    self.emit_usz(idx);
    if (pat.rest_pos == REST_START) {
        // [.. a b] — elements are at end of list.
        self.emit(" = aot::list_nth(");
        self.emit(val_name);
        self.emit(", aot::list_length(");
        self.emit(val_name);
        self.emit(") - ");
        self.emit_usz(pat.elem_count - idx);
        self.emit("); ");
        return;
    }
    // REST_NONE, REST_END, REST_MIDDLE — elements at start.
    self.emit(" = aot::list_nth(");
    self.emit(val_name);
    self.emit(", ");
    self.emit_usz(idx);
    self.emit("); ");
}

fn void Compiler.emit_pattern_seq_elem_binding(Compiler* self, Pattern* pat, usz idx) {
    if (pat.elements[idx].tag != PAT_VAR) return;
    self.emit_pattern_var_decl(pat.elements[idx].var_name);
    self.emit_symbol_name(pat.elements[idx].var_name);
    self.emit(" = _elem_");
    self.emit_usz(idx);
    self.emit("; ");
}

fn void Compiler.compile_pattern_bindings(Compiler* self, Pattern* pat, char[] val_name) {
    if (pat == null) return;

    switch (pat.tag) {
        case PAT_VAR:
            self.emit_pattern_var_assign(pat.var_name, val_name);

        case PAT_SEQ:
            // Bind sequence elements.
            for (usz i = 0; i < pat.elem_count; i++) {
                self.emit_pattern_seq_elem_access(pat, val_name, i);
                self.emit_pattern_seq_elem_binding(pat, i);
            }

            // Bind rest if REST_MIDDLE.
            if (pat.rest_pos == REST_MIDDLE) {
                self.emit_pattern_var_decl(pat.rest_binding);
                self.emit_symbol_name(pat.rest_binding);
                self.emit(" = aot::list_rest(");
                self.emit(val_name);
                self.emit(", ");
                self.emit_usz(pat.elem_count);
                self.emit("); ");
            }

        case PAT_AS:
            self.compile_pattern_bindings(pat.as_sub, val_name);
            self.emit_pattern_var_assign(pat.as_name, val_name);

        case PAT_GUARD:
            // Guard pattern: bind sub-pattern if present
            if (pat.guard_sub != null) {
                self.compile_pattern_bindings(pat.guard_sub, val_name);
            }

        default:
            return;
    }
}
