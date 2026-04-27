module lisp;

import std::io;
import main;

fn bool Compiler.pattern_has_guard(Compiler* self, Pattern* pat) {
    if (pat == null) return false;

    switch (pat.tag) {
        case PAT_WILDCARD:
        case PAT_VAR:
        case PAT_LIT:
        case PAT_QUOTE:
        case PAT_DICT:
            return false;

        case PAT_CONS:
            return self.pattern_has_guard(pat.car_pat) || self.pattern_has_guard(pat.cdr_pat);

        case PAT_CONSTRUCTOR:
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                if (self.pattern_has_guard(pat.ctor_sub_patterns[i])) return true;
            }
            return false;

        case PAT_SEQ:
            for (usz i = 0; i < pat.elem_count; i++) {
                if (self.pattern_has_guard(pat.elements[i])) return true;
            }
            return false;

        case PAT_AS:
            return self.pattern_has_guard(pat.as_sub);

        case PAT_GUARD:
            return true;

        default:
            self.set_compile_error("compiler: unknown pattern tag in AOT match guard scan");
            return false;
    }
}

fn void Compiler.compile_pattern_guards(
    Compiler* self,
    Pattern* pat,
    char[] val_name,
    usz result_id,
    char[] matched_name,
    char[] clause_ready_name
) {
    if (pat == null) return;

    switch (pat.tag) {
        case PAT_WILDCARD:
        case PAT_VAR:
        case PAT_LIT:
        case PAT_QUOTE:
        case PAT_DICT:
            return;

        case PAT_CONS:
            usz car_r = self.next_result();
            self.emit_temp_decl(car_r);
            self.emit(" = aot::car(");
            self.emit(val_name);
            self.emit(");\n");
            char[24] car_name;
            char[] car_name_s = self.generated_name_id(car_name[..], "_r", (long)car_r);
            self.compile_pattern_guards(pat.car_pat, car_name_s, result_id, matched_name, clause_ready_name);

            usz cdr_r = self.next_result();
            self.emit_temp_decl(cdr_r);
            self.emit(" = aot::cdr(");
            self.emit(val_name);
            self.emit(");\n");
            char[24] cdr_name;
            char[] cdr_name_s = self.generated_name_id(cdr_name[..], "_r", (long)cdr_r);
            self.compile_pattern_guards(pat.cdr_pat, cdr_name_s, result_id, matched_name, clause_ready_name);

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
                self.compile_pattern_guards(
                    pat.ctor_sub_patterns[i],
                    field_name_s,
                    result_id,
                    matched_name,
                    clause_ready_name
                );
            }

        case PAT_SEQ:
            for (usz i = 0; i < pat.elem_count; i++) {
                usz elem_r = self.next_result();
                self.emit_temp_decl(elem_r);
                self.emit(" = ");
                self.emit_pattern_seq_elem_expr(pat, val_name, i);
                self.emit(";\n");

                char[24] elem_name;
                char[] elem_name_s = self.generated_name_id(elem_name[..], "_r", (long)elem_r);
                self.compile_pattern_guards(
                    pat.elements[i],
                    elem_name_s,
                    result_id,
                    matched_name,
                    clause_ready_name
                );
            }

        case PAT_AS:
            self.compile_pattern_guards(pat.as_sub, val_name, result_id, matched_name, clause_ready_name);

        case PAT_GUARD:
            if (pat.guard_sub != null) {
                self.compile_pattern_guards(pat.guard_sub, val_name, result_id, matched_name, clause_ready_name);
            }

            self.emit_indent();
            self.emit("if (");
            self.emit(clause_ready_name);
            self.emit(") {\n");
            self.indent++;

            usz pred_r = self.compile_to_temp(pat.guard_pred);
            self.emit_return_if_sequence_should_stop(pred_r);
            usz guard_r = self.next_result();
            self.emit_temp_decl(guard_r);
            self.emit(" = aot::match_guard_eval(");
            self.emit_temp_ref(pred_r);
            self.emit(", ");
            self.emit(val_name);
            self.emit(");\n");

            self.emit_indent();
            self.emit("if (aot::is_error(");
            self.emit_temp_ref(guard_r);
            self.emit(")) {\n");
            self.indent++;
            self.emit_indent();
            self.emit_temp_ref(result_id);
            self.emit(" = ");
            self.emit_temp_ref(guard_r);
            self.emit(";\n");
            self.emit_indent();
            self.emit(matched_name);
            self.emit(" = true;\n");
            self.emit_indent();
            self.emit(clause_ready_name);
            self.emit(" = false;\n");
            self.indent--;
            self.emit_indent();
            self.emit("} else if (!aot::is_truthy(");
            self.emit_temp_ref(guard_r);
            self.emit(")) {\n");
            self.indent++;
            self.emit_indent();
            self.emit(clause_ready_name);
            self.emit(" = false;\n");
            self.indent--;
            self.emit_indent();
            self.emit("}\n");

            self.indent--;
            self.emit_indent();
            self.emit("}\n");

        default:
            self.set_compile_error("compiler: unknown pattern tag in AOT match guard lowering");
            return;
    }
}
