module lisp;

import std::io;

fn void Compiler.compile_pattern_check_to_bool(Compiler* self, Pattern* pat, char[] val_name, char[] out_name) {
    if (pat == null) {
        self.emit_indent();
        self.emit("bool ");
        self.emit(out_name);
        self.emit(" = true;\n");
        return;
    }

    switch (pat.tag) {
        case PAT_WILDCARD:
            self.emit_indent();
            self.emit("bool ");
            self.emit(out_name);
            self.emit(" = true;\n");

        case PAT_VAR:
            self.emit_indent();
            self.emit("bool ");
            self.emit(out_name);
            self.emit(" = aot::match_var_pattern(");
            self.emit(val_name);
            self.emit(", ");
            self.emit_string_literal(self.interp.symbols.get_name(pat.var_name));
            self.emit(");\n");

        case PAT_LIT:
            self.emit_indent();
            self.emit("bool ");
            self.emit(out_name);
            self.emit(" = aot::values_equal(");
            self.emit(val_name);
            self.emit(", ");
            self.compile_literal(pat.lit_value);
            self.emit(");\n");

        case PAT_QUOTE:
            self.emit_indent();
            self.emit("bool ");
            self.emit(out_name);
            self.emit(" = aot::values_equal(");
            self.emit(val_name);
            self.emit(", ");
            self.compile_literal(pat.quote_datum);
            self.emit(");\n");

        case PAT_CONS:
            self.emit_indent();
            self.emit("bool ");
            self.emit(out_name);
            self.emit(" = aot::is_cons(");
            self.emit(val_name);
            self.emit(");\n");

            self.emit_indent();
            self.emit("if (");
            self.emit(out_name);
            self.emit(") {\n");
            self.indent++;

            usz car_r = self.next_result();
            self.emit_temp_decl(car_r);
            self.emit(" = aot::car(");
            self.emit(val_name);
            self.emit(");\n");
            char[24] car_name;
            char[] car_name_s = compiler_format_generated_name_id(car_name[..], "_r", (long)car_r);
            char[32] car_check_name;
            char[] car_check_name_s = compiler_format_generated_name_id(car_check_name[..], "_match_pat_car_", (long)car_r);
            self.compile_pattern_check_to_bool(pat.car_pat, car_name_s, car_check_name_s);
            self.emit_indent();
            self.emit(out_name);
            self.emit(" = ");
            self.emit(car_check_name_s);
            self.emit(";\n");

            self.emit_indent();
            self.emit("if (");
            self.emit(out_name);
            self.emit(") {\n");
            self.indent++;
            usz cdr_r = self.next_result();
            self.emit_temp_decl(cdr_r);
            self.emit(" = aot::cdr(");
            self.emit(val_name);
            self.emit(");\n");
            char[24] cdr_name;
            char[] cdr_name_s = compiler_format_generated_name_id(cdr_name[..], "_r", (long)cdr_r);
            char[32] cdr_check_name;
            char[] cdr_check_name_s = compiler_format_generated_name_id(cdr_check_name[..], "_match_pat_cdr_", (long)cdr_r);
            self.compile_pattern_check_to_bool(pat.cdr_pat, cdr_name_s, cdr_check_name_s);
            self.emit_indent();
            self.emit(out_name);
            self.emit(" = ");
            self.emit(cdr_check_name_s);
            self.emit(";\n");
            self.indent--;
            self.emit_indent();
            self.emit("}\n");

            self.indent--;
            self.emit_indent();
            self.emit("}\n");

        case PAT_SEQ:
            usz seq_len_id = self.next_result();
            char[32] seq_len_name;
            char[] seq_len_name_s = compiler_format_generated_name_id(seq_len_name[..], "_match_seq_len_", (long)seq_len_id);
            self.emit_indent();
            self.emit("usz ");
            self.emit(seq_len_name_s);
            self.emit(" = aot::seq_length(");
            self.emit(val_name);
            self.emit(");\n");

            self.emit_indent();
            self.emit("bool ");
            self.emit(out_name);
            self.emit(" = ");
            if (pat.rest_pos == REST_NONE) {
                self.emit(seq_len_name_s);
                self.emit(" == ");
                self.emit_usz(pat.elem_count);
            } else if (pat.rest_pos == REST_START || pat.rest_pos == REST_MIDDLE || pat.rest_pos == REST_END) {
                self.emit(seq_len_name_s);
                self.emit(" >= ");
                self.emit_usz(pat.elem_count);
            }
            self.emit(";\n");

            for (usz i = 0; i < pat.elem_count; i++) {
                self.emit_indent();
                self.emit("if (");
                self.emit(out_name);
                self.emit(") {\n");
                self.indent++;
                usz elem_r = self.next_result();
                self.emit_temp_decl(elem_r);
                self.emit(" = ");
                self.emit_pattern_seq_elem_expr(pat, val_name, i);
                self.emit(";\n");

                char[24] elem_name;
                char[] elem_name_s = compiler_format_generated_name_id(elem_name[..], "_r", (long)elem_r);
                char[32] elem_check_name;
                char[] elem_check_name_s = compiler_format_generated_name_id(elem_check_name[..], "_match_seq_pat_", (long)elem_r);
                self.compile_pattern_check_to_bool(pat.elements[i], elem_name_s, elem_check_name_s);
                self.emit_indent();
                self.emit(out_name);
                self.emit(" = ");
                self.emit(elem_check_name_s);
                self.emit(";\n");
                self.indent--;
                self.emit_indent();
                self.emit("}\n");
            }

        case PAT_AS:
            self.compile_pattern_check_to_bool(pat.as_sub, val_name, out_name);

        case PAT_GUARD:
            if (pat.guard_sub != null) {
                self.compile_pattern_check_to_bool(pat.guard_sub, val_name, out_name);
            } else {
                self.emit_indent();
                self.emit("bool ");
                self.emit(out_name);
                self.emit(" = true;\n");
            }

        case PAT_CONSTRUCTOR:
            self.emit_indent();
            self.emit("bool ");
            self.emit(out_name);
            self.emit(" = aot::match_constructor(");
            self.emit(val_name);
            self.emit(", ");
            self.emit_string_literal(self.interp.symbols.get_name(pat.constructor_name));
            self.emit(", ");
            self.emit_usz(pat.ctor_sub_count);
            self.emit(");\n");

            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                self.emit_indent();
                self.emit("if (");
                self.emit(out_name);
                self.emit(") {\n");
                self.indent++;
                usz field_r = self.next_result();
                self.emit_temp_decl(field_r);
                self.emit(" = aot::instance_field(");
                self.emit(val_name);
                self.emit(", ");
                self.emit_usz(i);
                self.emit(");\n");

                char[24] field_name;
                char[] field_name_s = compiler_format_generated_name_id(field_name[..], "_r", (long)field_r);
                char[32] field_check_name;
                char[] field_check_name_s = compiler_format_generated_name_id(field_check_name[..], "_match_ctor_pat_", (long)field_r);
                self.compile_pattern_check_to_bool(
                    pat.ctor_sub_patterns[i],
                    field_name_s,
                    field_check_name_s
                );
                self.emit_indent();
                self.emit(out_name);
                self.emit(" = ");
                self.emit(field_check_name_s);
                self.emit(";\n");
                self.indent--;
                self.emit_indent();
                self.emit("}\n");
            }

        case PAT_DICT:
            self.emit_indent();
            self.emit("bool ");
            self.emit(out_name);
            self.emit(" = aot::is_dict(");
            self.emit(val_name);
            self.emit(");\n");

        default:
            self.emit_indent();
            self.emit("bool ");
            self.emit(out_name);
            self.emit(" = true;\n");
    }
}
