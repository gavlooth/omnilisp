module lisp;

import main;
// =============================================================================
// SECTION: NATIVE CALL COMPILATION (flat style)
// =============================================================================

fn usz Compiler.compile_lambda_flat(Compiler* self, Expr* expr) {
    // Find the lambda definition we created earlier
    usz lambda_id = 0;
    LambdaDef* found_def = null;

    foreach (&def : self.lambda_defs) {
        if (def.body == expr.lambda.body && (uint)def.param == (uint)expr.lambda.param) {
            lambda_id = def.id;
            found_def = def;
            break;
        }
    }

    if (found_def != null && found_def.capture_count > 0) {
        // Closure with captures - allocate on heap
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

        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::");
        self.emit_make_closure_call(lambda_id);
        self.emit("((void*)_closure_data_");
        self.emit_usz(lambda_id);
        self.emit(", &invoke_lambda_");
        self.emit_usz(lambda_id);
        self.emit(");\n");
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

fn void Compiler.compile_var(Compiler* self, Expr* expr) {
    SymbolId name = expr.var_expr.name;

    // O(1) hash lookup for known primitives and literals
    char[] emit_code = prim_hash_lookup(name);
    if (emit_code.len > 0) {
        // D2: For actual primitives (lookup_prim calls), use cached global instead.
        // Literals (true/false/nil) are cheap constructors — emit inline.
        if (emit_code.len > 18 && emit_code[0] == 'a' && emit_code[4] == ':') {
            // Starts with "aot::" — it's a lookup_prim call, use cached global
            self.record_prim_ref(name);
            self.emit_prim_global_name(name);
        } else {
            // Literal (make_true, make_false, make_nil) — emit inline
            self.emit(emit_code);
        }
        return;
    }

    // Mutable-captured variable: look up from interpreter env
    if (self.is_mutable_captured_var(name)) {
        char[] sym_name = self.interp.symbols.get_name(name);
        self.emit("aot::lookup_var(\"");
        self.emit_escaped(sym_name);
        self.emit("\")");
        return;
    }

    // Regular variable reference
    self.emit_symbol_name(name);
}

/**
 * compile_path — Field access: point.x, person.address.city
 * person.address.city => rt_field_access(rt_field_access(person, "address"), "city")
 */
fn void Compiler.compile_path(Compiler* self, Expr* expr) {
    if (expr.path.segment_count == 0) {
        self.emit("aot::make_nil()");
        return;
    }

    // First segment is a variable, remaining segments are field lookups
    for (usz i = 1; i < expr.path.segment_count; i++) {
        self.emit("aot::field_access(");
    }

    // Emit first segment as variable reference
    self.emit_symbol_name(expr.path.segments[0]);

    // Emit remaining segments as string field names (wrapped in make_string for Value type)
    for (usz i = 1; i < expr.path.segment_count; i++) {
        self.emit(", aot::make_string(\"");
        char[] name = self.interp.symbols.get_name(expr.path.segments[i]);
        self.emit_escaped(name);
        self.emit("\"))");
    }
}
