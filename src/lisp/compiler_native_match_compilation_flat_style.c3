module lisp;

import std::io;
import main;
// =============================================================================
// SECTION: NATIVE MATCH/PATTERN COMPILATION (flat style)
// =============================================================================

fn usz Compiler.compile_match_flat(Compiler* self, Expr* expr) {
    // Compile scrutinee to temp
    usz scrutinee_r = self.compile_to_temp(expr.match.scrutinee);

    // Declare result temp
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_nil();\n");

    char[24] matched_name;
    usz matched_len = io::bprintf(&matched_name, "_match_done_%d", (int)id)!!.len;
    self.emit_indent();
    self.emit("bool ");
    self.emit(matched_name[:matched_len]);
    self.emit(" = false;\n");

    // Use temp ref name for pattern checks and guard application.
    char[16] ref_name;
    usz ref_len = io::bprintf(&ref_name, "_r%d", (int)scrutinee_r)!!.len;

    for (usz i = 0; i < expr.match.clause_count; i++) {
        char[32] clause_check_name;
        usz clause_check_len = io::bprintf(&clause_check_name, "_match_clause_pat_%d_%d", (int)id, (int)i)!!.len;
        self.compile_pattern_check_to_bool(
            expr.match.clauses[i].pattern,
            ref_name[:ref_len],
            clause_check_name[:clause_check_len]
        );

        self.emit_indent();
        self.emit("if (!");
        self.emit(matched_name[:matched_len]);
        self.emit(" && ");
        self.emit(clause_check_name[:clause_check_len]);
        self.emit(") {\n");
        self.indent++;

        // Bind pattern variables
        self.compile_pattern_bindings(expr.match.clauses[i].pattern, ref_name[:ref_len]);

        bool has_guard = self.pattern_has_guard(expr.match.clauses[i].pattern);
        char[32] clause_ready_name;
        usz clause_ready_len = 0;
        if (has_guard) {
            clause_ready_len = io::bprintf(&clause_ready_name, "_match_clause_ok_%d_%d", (int)id, (int)i)!!.len;
            self.emit_indent();
            self.emit("bool ");
            self.emit(clause_ready_name[:clause_ready_len]);
            self.emit(" = true;\n");
            self.compile_pattern_guards(
                expr.match.clauses[i].pattern,
                ref_name[:ref_len],
                id,
                matched_name[:matched_len],
                clause_ready_name[:clause_ready_len]
            );
            self.emit_indent();
            self.emit("if (");
            self.emit(clause_ready_name[:clause_ready_len]);
            self.emit(") {\n");
            self.indent++;
        }

        // Compile result
        usz clause_r = self.compile_to_temp(expr.match.clauses[i].result);
        self.emit_indent();
        self.emit_temp_ref(id);
        self.emit(" = ");
        self.emit_temp_ref(clause_r);
        self.emit(";\n");
        self.emit_indent();
        self.emit(matched_name[:matched_len]);
        self.emit(" = true;\n");
        if (has_guard) {
            self.indent--;
            self.emit_indent();
            self.emit("}\n");
        }
        self.indent--;
        self.emit_indent();
        self.emit("}\n");
    }

    return id;
}

fn bool Compiler.pattern_has_guard(Compiler* self, Pattern* pat) {
    (void)self;
    if (pat == null) return false;

    switch (pat.tag) {
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
        case PAT_CONS:
            usz car_r = self.next_result();
            self.emit_temp_decl(car_r);
            self.emit(" = aot::car(");
            self.emit(val_name);
            self.emit(");\n");
            char[24] car_name;
            usz car_len = io::bprintf(&car_name, "_r%d", (int)car_r)!!.len;
            self.compile_pattern_guards(pat.car_pat, car_name[:car_len], result_id, matched_name, clause_ready_name);

            usz cdr_r = self.next_result();
            self.emit_temp_decl(cdr_r);
            self.emit(" = aot::cdr(");
            self.emit(val_name);
            self.emit(");\n");
            char[24] cdr_name;
            usz cdr_len = io::bprintf(&cdr_name, "_r%d", (int)cdr_r)!!.len;
            self.compile_pattern_guards(pat.cdr_pat, cdr_name[:cdr_len], result_id, matched_name, clause_ready_name);

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
                usz field_len = io::bprintf(&field_name, "_r%d", (int)field_r)!!.len;
                self.compile_pattern_guards(
                    pat.ctor_sub_patterns[i],
                    field_name[:field_len],
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
                usz elem_len = io::bprintf(&elem_name, "_r%d", (int)elem_r)!!.len;
                self.compile_pattern_guards(
                    pat.elements[i],
                    elem_name[:elem_len],
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
            return;
    }
}

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
            usz car_len = io::bprintf(&car_name, "_r%d", (int)car_r)!!.len;
            char[32] car_check_name;
            usz car_check_len = io::bprintf(&car_check_name, "_match_pat_car_%d", (int)car_r)!!.len;
            self.compile_pattern_check_to_bool(pat.car_pat, car_name[:car_len], car_check_name[:car_check_len]);
            self.emit_indent();
            self.emit(out_name);
            self.emit(" = ");
            self.emit(car_check_name[:car_check_len]);
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
            usz cdr_len = io::bprintf(&cdr_name, "_r%d", (int)cdr_r)!!.len;
            char[32] cdr_check_name;
            usz cdr_check_len = io::bprintf(&cdr_check_name, "_match_pat_cdr_%d", (int)cdr_r)!!.len;
            self.compile_pattern_check_to_bool(pat.cdr_pat, cdr_name[:cdr_len], cdr_check_name[:cdr_check_len]);
            self.emit_indent();
            self.emit(out_name);
            self.emit(" = ");
            self.emit(cdr_check_name[:cdr_check_len]);
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
            usz seq_len_len = io::bprintf(&seq_len_name, "_match_seq_len_%d", (int)seq_len_id)!!.len;
            self.emit_indent();
            self.emit("usz ");
            self.emit(seq_len_name[:seq_len_len]);
            self.emit(" = aot::seq_length(");
            self.emit(val_name);
            self.emit(");\n");

            self.emit_indent();
            self.emit("bool ");
            self.emit(out_name);
            self.emit(" = ");
            if (pat.rest_pos == REST_NONE) {
                self.emit(seq_len_name[:seq_len_len]);
                self.emit(" == ");
                self.emit_usz(pat.elem_count);
            } else if (pat.rest_pos == REST_START || pat.rest_pos == REST_MIDDLE || pat.rest_pos == REST_END) {
                self.emit(seq_len_name[:seq_len_len]);
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
                usz elem_len = io::bprintf(&elem_name, "_r%d", (int)elem_r)!!.len;
                char[32] elem_check_name;
                usz elem_check_len = io::bprintf(&elem_check_name, "_match_seq_pat_%d", (int)elem_r)!!.len;
                self.compile_pattern_check_to_bool(pat.elements[i], elem_name[:elem_len], elem_check_name[:elem_check_len]);
                self.emit_indent();
                self.emit(out_name);
                self.emit(" = ");
                self.emit(elem_check_name[:elem_check_len]);
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
                usz field_len = io::bprintf(&field_name, "_r%d", (int)field_r)!!.len;
                char[32] field_check_name;
                usz field_check_len = io::bprintf(&field_check_name, "_match_ctor_pat_%d", (int)field_r)!!.len;
                self.compile_pattern_check_to_bool(
                    pat.ctor_sub_patterns[i],
                    field_name[:field_len],
                    field_check_name[:field_check_len]
                );
                self.emit_indent();
                self.emit(out_name);
                self.emit(" = ");
                self.emit(field_check_name[:field_check_len]);
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
