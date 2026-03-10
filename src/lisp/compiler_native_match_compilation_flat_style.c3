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

    for (usz i = 0; i < expr.match.clause_count; i++) {
        self.emit_indent();
        if (i > 0) {
            self.emit("else ");
        }
        self.emit("if (");
        // Use temp ref name for pattern checks
        char[16] ref_name;
        usz ref_len = io::bprintf(&ref_name, "_r%d", (int)scrutinee_r)!!.len;
        self.compile_pattern_check(expr.match.clauses[i].pattern, ref_name[:ref_len]);
        self.emit(") {\n");
        self.indent++;

        // Bind pattern variables
        self.emit_indent();
        self.compile_pattern_bindings(expr.match.clauses[i].pattern, ref_name[:ref_len]);

        // Compile result
        usz clause_r = self.compile_to_temp(expr.match.clauses[i].result);
        self.emit_indent();
        self.emit_temp_ref(id);
        self.emit(" = ");
        self.emit_temp_ref(clause_r);
        self.emit(";\n");
        self.indent--;
        self.emit_indent();
        self.emit("} ");
    }

    self.emit("\n");
    return id;
}

fn void Compiler.compile_pattern_check(Compiler* self, Pattern* pat, char[] val_name) {
    if (pat == null) {
        self.emit("true");
        return;
    }

    switch (pat.tag) {
        case PAT_WILDCARD:
            self.emit("true");

        case PAT_VAR:
            self.emit("true");

        case PAT_LIT:
            self.emit("aot::values_equal(");
            self.emit(val_name);
            self.emit(", ");
            self.compile_literal(pat.lit_value);
            self.emit(")");

        case PAT_QUOTE:
            self.emit("aot::values_equal(");
            self.emit(val_name);
            self.emit(", ");
            self.compile_literal(pat.quote_datum);
            self.emit(")");

        case PAT_SEQ:
            // Check list length and elements
            self.emit("aot::is_list(");
            self.emit(val_name);
            self.emit(")");

            if (pat.rest_pos == REST_NONE) {
                // Exact length match
                self.emit(" && aot::list_length(");
                self.emit(val_name);
                self.emit(") == ");
                self.emit_usz(pat.elem_count);
            } else if (pat.rest_pos == REST_START || pat.rest_pos == REST_MIDDLE || pat.rest_pos == REST_END) {
                // At least N elements
                self.emit(" && aot::list_length(");
                self.emit(val_name);
                self.emit(") >= ");
                self.emit_usz(pat.elem_count);
            }

        case PAT_AS:
            self.compile_pattern_check(pat.as_sub, val_name);

        case PAT_GUARD:
            // Guard patterns: delegate to true for now (guard check at runtime)
            self.emit("true");

        default:
            self.emit("true");
    }
}
