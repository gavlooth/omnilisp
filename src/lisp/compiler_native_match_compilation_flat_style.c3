module lisp;

import std::io;
import main;
// =============================================================================
// SECTION: NATIVE MATCH/PATTERN COMPILATION (flat style)
// =============================================================================

fn usz Compiler.compile_match_flat(Compiler* self, Expr* expr) {
    // Compile scrutinee to temp
    usz scrutinee_r = self.compile_to_temp(expr.match.scrutinee);
    self.emit_return_if_sequence_should_stop(scrutinee_r);

    // Declare result temp
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_nil();\n");

    char[24] matched_name;
    char[] matched_name_s = self.generated_name_id(matched_name[..], "_match_done_", (long)id);
    usz matched_len = matched_name_s.len;
    self.emit_indent();
    self.emit("bool ");
    self.emit(matched_name[:matched_len]);
    self.emit(" = false;\n");

    // Use temp ref name for pattern checks and guard application.
    char[16] ref_name;
    char[] ref_name_s = self.generated_name_id(ref_name[..], "_r", (long)scrutinee_r);
    usz ref_len = ref_name_s.len;

    for (usz i = 0; i < expr.match.clause_count; i++) {
        char[32] clause_check_name;
        char[] clause_check_name_s = self.generated_name_pair(clause_check_name[..], "_match_clause_pat_", (long)id, (long)i);
        usz clause_check_len = clause_check_name_s.len;
        self.compile_pattern_check_to_bool(
            expr.match.clauses[i].pattern,
            ref_name_s[:ref_len],
            clause_check_name_s[:clause_check_len]
        );

        self.emit_indent();
        self.emit("if (!");
        self.emit(matched_name_s[:matched_len]);
        self.emit(" && ");
        self.emit(clause_check_name_s[:clause_check_len]);
        self.emit(") {\n");
        self.indent++;
        usz declared_mark = self.mark_declared_scope();

        // Bind pattern variables
        self.compile_pattern_bindings(expr.match.clauses[i].pattern, ref_name_s[:ref_len]);

        bool has_guard = self.pattern_has_guard(expr.match.clauses[i].pattern);
        char[32] clause_ready_name;
        usz clause_ready_len = 0;
        if (has_guard) {
            char[32] clause_ready_name_buf;
            char[] clause_ready_name_s = self.generated_name_pair(clause_ready_name_buf[..], "_match_clause_ok_", (long)id, (long)i);
            clause_ready_len = clause_ready_name_s.len;
            self.emit_indent();
            self.emit("bool ");
            self.emit(clause_ready_name_s[:clause_ready_len]);
            self.emit(" = true;\n");
            self.compile_pattern_guards(
                expr.match.clauses[i].pattern,
                ref_name_s[:ref_len],
                id,
                matched_name_s[:matched_len],
                clause_ready_name_s[:clause_ready_len]
            );
            self.emit_indent();
            self.emit("if (");
            self.emit(clause_ready_name_s[:clause_ready_len]);
            self.emit(") {\n");
            self.indent++;
        }

        // Compile result
        usz clause_r = self.compile_to_temp(expr.match.clauses[i].result);
        self.emit_return_if_sequence_should_stop(clause_r);
        self.emit_indent();
        self.emit_temp_ref(id);
        self.emit(" = ");
        self.emit_temp_ref(clause_r);
        self.emit(";\n");
        self.emit_indent();
        self.emit(matched_name_s[:matched_len]);
        self.emit(" = true;\n");
        if (has_guard) {
            self.indent--;
            self.emit_indent();
            self.emit("}\n");
        }
        self.indent--;
        self.emit_indent();
        self.emit("}\n");
        self.restore_declared_scope(declared_mark);
    }

    return id;
}
