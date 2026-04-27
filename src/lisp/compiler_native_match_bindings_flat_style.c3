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

fn void Compiler.emit_pattern_seq_elem_expr(Compiler* self, Pattern* pat, char[] val_name, usz idx) {
    self.emit("aot::seq_nth(");
    self.emit(val_name);
    if (pat.rest_pos == REST_START) {
        self.emit(", aot::seq_length(");
        self.emit(val_name);
        self.emit(") - ");
        self.emit_usz(pat.elem_count - idx);
        self.emit(")");
        return;
    }
    self.emit(", ");
    self.emit_usz(idx);
    self.emit(")");
}

fn void Compiler.compile_pattern_bindings(Compiler* self, Pattern* pat, char[] val_name) {
    if (pat == null) return;

    switch (pat.tag) {
        case PAT_VAR:
            self.emit_indent();
            self.emit_pattern_var_assign(pat.var_name, val_name);
            self.emit("\n");

        case PAT_SEQ:
            for (usz i = 0; i < pat.elem_count; i++) {
                usz elem_r = self.next_result();
                self.emit_temp_decl(elem_r);
                self.emit(" = ");
                self.emit_pattern_seq_elem_expr(pat, val_name, i);
                self.emit(";\n");

                char[24] elem_name;
                char[] elem_name_s = self.generated_name_id(elem_name[..], "_r", (long)elem_r);
                self.compile_pattern_bindings(pat.elements[i], elem_name_s);
            }

            if (pat.rest_pos == REST_MIDDLE) {
                self.emit_indent();
                self.emit_pattern_var_decl(pat.rest_binding);
                self.emit_symbol_name(pat.rest_binding);
                self.emit(" = aot::seq_rest(");
                self.emit(val_name);
                self.emit(", ");
                self.emit_usz(pat.elem_count);
                self.emit(");\n");
            }

        case PAT_CONS:
            usz car_r = self.next_result();
            self.emit_temp_decl(car_r);
            self.emit(" = aot::car(");
            self.emit(val_name);
            self.emit(");\n");
            char[24] car_name;
            char[] car_name_s = self.generated_name_id(car_name[..], "_r", (long)car_r);
            self.compile_pattern_bindings(pat.car_pat, car_name_s);

            usz cdr_r = self.next_result();
            self.emit_temp_decl(cdr_r);
            self.emit(" = aot::cdr(");
            self.emit(val_name);
            self.emit(");\n");
            char[24] cdr_name;
            char[] cdr_name_s = self.generated_name_id(cdr_name[..], "_r", (long)cdr_r);
            self.compile_pattern_bindings(pat.cdr_pat, cdr_name_s);

        case PAT_CONSTRUCTOR:
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                usz field_r = self.next_result();
                self.emit_temp_decl(field_r);
                self.emit(" = aot::instance_field(");
                self.emit(val_name);
                self.emit(", ");
                self.emit_usz(i);
                self.emit(");\n");

                char[24] field_name;
                char[] field_name_s = self.generated_name_id(field_name[..], "_r", (long)field_r);
                self.compile_pattern_bindings(pat.ctor_sub_patterns[i], field_name_s);
            }

        case PAT_AS:
            self.compile_pattern_bindings(pat.as_sub, val_name);
            self.emit_indent();
            self.emit_pattern_var_assign(pat.as_name, val_name);
            self.emit("\n");

        case PAT_GUARD:
            if (pat.guard_sub != null) {
                self.compile_pattern_bindings(pat.guard_sub, val_name);
            }

        case PAT_DICT:
            for (usz i = 0; i < pat.dict_key_count; i++) {
                self.emit_indent();
                self.emit_pattern_var_decl(pat.dict_keys[i]);
                self.emit_symbol_name(pat.dict_keys[i]);
                self.emit(" = aot::dict_get_symbol(");
                self.emit(val_name);
                self.emit(", ");
                self.emit_string_literal(self.interp.symbols.get_name(pat.dict_keys[i]));
                self.emit(");\n");
            }

        case PAT_WILDCARD:
        case PAT_LIT:
        case PAT_QUOTE:
            return;

        default:
            self.set_compile_error("compiler: unknown pattern tag in AOT match binding lowering");
            return;
    }
}
