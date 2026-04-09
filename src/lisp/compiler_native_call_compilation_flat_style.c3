module lisp;

import main;
// =============================================================================
// SECTION: NATIVE CALL COMPILATION (flat style)
// =============================================================================

fn usz Compiler.compile_lambda_flat(Compiler* self, Expr* expr) {
    // Find the lambda definition we created earlier
    usz lambda_id = 0;
    LambdaDef* found_def = null;

    if (!g_compiler_force_lambda_lookup_miss) {
        foreach (&def : self.lambda_defs) {
            if (def.expr == expr) {
                lambda_id = def.id;
                found_def = def;
                break;
            }
        }
    }

    if (found_def == null) {
        self.set_compile_error("compiler: missing lambda definition during AOT lambda lowering");
        return self.emit_nil_temp();
    }

    if (found_def != null && found_def.capture_count > 0) {
        // Closure with captures - allocate on heap
        usz id = self.next_result();
        self.emit_indent();
        self.emit("Lambda_");
        self.emit_usz(lambda_id);
        self.emit("* _closure_data_");
        self.emit_usz(lambda_id);
        self.emit(" = (Lambda_");
        self.emit_usz(lambda_id);
        self.emit("*)mem::malloc(Lambda_");
        self.emit_usz(lambda_id);
        self.emit(".sizeof);\n");
        self.emit_indent();
        self.emit_temp_decl(id);
        self.emit(";\n");
        self.emit_indent();
        self.emit("if (_closure_data_");
        self.emit_usz(lambda_id);
        self.emit(" == null) {\n");
        self.indent++;
        self.emit_indent();
        self.emit_temp_ref(id);
        self.emit(" = aot::make_error(\"compiler: out of memory while allocating closure capture data\");\n");
        self.indent--;
        self.emit_indent();
        self.emit("} else {\n");
        self.indent++;

        // Set captured values
        for (usz i = 0; i < found_def.capture_count; i++) {
        self.emit_indent();
        self.emit("_closure_data_");
        self.emit_usz(lambda_id);
        self.emit(".captured_");
            self.emit_symbol_name(found_def.captures[i]);
            self.emit(" = ");
            self.emit_symbol_name(found_def.captures[i]);
            self.emit(";\n");
        }

        self.emit_indent();
        self.emit_temp_ref(id);
        self.emit(" = aot::");
        self.emit_make_closure_call(lambda_id);
        self.emit("((void*)_closure_data_");
        self.emit_usz(lambda_id);
        self.emit(", &invoke_lambda_");
        self.emit_usz(lambda_id);
        self.emit(");\n");
        self.emit_indent();
        self.emit("if (");
        self.emit_temp_ref(id);
        self.emit(" != null && ");
        self.emit_temp_ref(id);
        self.emit(".tag == lisp::ValueTag.ERROR) mem::free(_closure_data_");
        self.emit_usz(lambda_id);
        self.emit(");\n");
        self.indent--;
        self.emit_indent();
        self.emit("}\n");
        return id;
    } else {
        // Simple closure without captures
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::");
        self.emit_make_closure_call(lambda_id);
        self.emit("(null, &invoke_lambda_");
        self.emit_usz(lambda_id);
        self.emit(");\n");
        return id;
    }
}
