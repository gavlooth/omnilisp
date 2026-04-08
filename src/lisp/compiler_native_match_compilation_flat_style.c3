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
