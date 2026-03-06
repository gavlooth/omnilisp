module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION: NATIVE EFFECT COMPILATION (flat style)
// =============================================================================

fn usz Compiler.compile_reset_flat(Compiler* self, Expr* expr) {
    // Body has been wrapped as a lambda during prescan
    usz body_r = self.compile_to_temp(expr.reset.body);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::compiled_reset(");
    self.emit_temp_ref(body_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_shift_flat(Compiler* self, Expr* expr) {
    // Body has been wrapped as lambda (lambda (k) ...) during prescan
    usz body_r = self.compile_to_temp(expr.shift.body);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::compiled_shift(");
    self.emit_temp_ref(body_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_perform_flat(Compiler* self, Expr* expr) {
    // Compile the argument
    usz arg_r = self.compile_to_temp(expr.perform.arg);
    // Create tag symbol
    usz tag_r = self.next_result();
    self.emit_temp_decl(tag_r);
    self.emit(" = aot::make_symbol(\"");
    char[] tag_name = self.interp.symbols.get_name(expr.perform.tag);
    self.emit_escaped(tag_name);
    self.emit("\");\n");

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::compiled_signal(");
    self.emit_temp_ref(tag_r);
    self.emit(", ");
    self.emit_temp_ref(arg_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_handle_effect_tag(Compiler* self, SymbolId effect_tag) {
    usz tag_r = self.next_result();
    self.emit_temp_decl(tag_r);
    self.emit(" = aot::make_symbol(\"");
    char[] tag_name = self.interp.symbols.get_name(effect_tag);
    self.emit_escaped(tag_name);
    self.emit("\");\n");
    return tag_r;
}

fn void Compiler.emit_handle_arrays(Compiler* self, usz clause_count, usz* tags_arr_out, usz* hdlrs_arr_out) {
    *tags_arr_out = self.next_result();
    self.emit_indent();
    self.emit("lisp::Value*[");
    self.emit_usz(clause_count > 0 ? clause_count : 1);
    self.emit("] _eff_tags_");
    self.emit_usz(*tags_arr_out);
    self.emit(";\n");

    *hdlrs_arr_out = self.next_result();
    self.emit_indent();
    self.emit("lisp::Value*[");
    self.emit_usz(clause_count > 0 ? clause_count : 1);
    self.emit("] _eff_hdlrs_");
    self.emit_usz(*hdlrs_arr_out);
    self.emit(";\n");
}

fn void Compiler.emit_handle_array_slot(Compiler* self, char[] name, usz arr_id, usz idx, usz value_r) {
    self.emit_indent();
    self.emit(name);
    self.emit_usz(arr_id);
    self.emit("[");
    self.emit_usz(idx);
    self.emit("] = ");
    self.emit_temp_ref(value_r);
    self.emit(";\n");
}

fn usz Compiler.compile_handle_flat(Compiler* self, Expr* expr) {
    usz clause_count = expr.handle.clause_count;

    // Compile all handler closures and tags
    usz[8] handler_rs;
    usz[8] tag_rs;
    for (usz i = 0; i < clause_count && i < 8; i++) {
        tag_rs[i] = self.compile_handle_effect_tag(expr.handle.clauses[i].effect_tag);
        // Handler closure (synthetic lambda from prescan)
        handler_rs[i] = self.compile_to_temp(expr.handle.clauses[i].handler_body);
    }

    // Compile body closure (synthetic lambda from prescan)
    usz body_r = self.compile_to_temp(expr.handle.body);

    // Build tags and handlers arrays (C3 array syntax: Type[N] name).
    usz tags_arr = 0;
    usz hdlrs_arr = 0;
    self.emit_handle_arrays(clause_count, &tags_arr, &hdlrs_arr);

    for (usz i = 0; i < clause_count && i < 8; i++) {
        self.emit_handle_array_slot("_eff_tags_", tags_arr, i, tag_rs[i]);
        self.emit_handle_array_slot("_eff_hdlrs_", hdlrs_arr, i, handler_rs[i]);
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::compiled_handle(&_eff_tags_");
    self.emit_usz(tags_arr);
    self.emit(", &_eff_hdlrs_");
    self.emit_usz(hdlrs_arr);
    self.emit(", ");
    self.emit_usz(clause_count);
    self.emit(", ");
    self.emit_temp_ref(body_r);
    self.emit(");\n");
    return id;
}

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

fn void Compiler.compile_literal(Compiler* self, Value* v) {
    if (v == null || v.tag == NIL) {
        self.emit("aot::make_nil()");
        return;
    }

    switch (v.tag) {
        case INT:
            self.emit("aot::make_int(");
            self.emit_int(v.int_val);
            self.emit(")");

        case DOUBLE:
            self.emit("aot::make_double(");
            char[64] dbuf;
            char[] dslice = io::bprintf(&dbuf, "%.17g", v.double_val)!!;
            self.emit(dslice);
            self.emit(")");

        case STRING:
            self.emit("aot::make_string(\"");
            // Emit string with escaping
            for (usz i = 0; i < v.str_len; i++) {
                char c = v.str_chars[i];
                if (c == '"') {
                    self.emit("\\\"");
                } else if (c == '\\') {
                    self.emit("\\\\");
                } else if (c == '\n') {
                    self.emit("\\n");
                } else if (c == '\t') {
                    self.emit("\\t");
                } else {
                    self.emit_char(c);
                }
            }
            self.emit("\")");

        case SYMBOL:
            self.emit("aot::make_symbol(\"");
            char[] name = self.interp.symbols.get_name(v.sym_val);
            self.emit_escaped(name);
            self.emit("\")");

        case CONS:
            self.emit("aot::cons(");
            self.compile_literal(v.cons_val.car);
            self.emit(", ");
            self.compile_literal(v.cons_val.cdr);
            self.emit(")");

        default:
            self.emit("aot::make_nil()");
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

fn void Compiler.compile_quote(Compiler* self, Value* datum) {
    // Quote compiles to literal value construction
    self.compile_literal(datum);
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
