module lisp;

import std::io;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (direct type definitions)
// =============================================================================
fn usz Compiler.compile_deftype_direct(Compiler* self, Expr* expr) {
    ExprDefType* dt = expr.deftype;
    usz fields_id = self.next_result();
    char[64] fields_name_buf;
    char[] fields_name = io::bprintf(&fields_name_buf, "_aot_type_fields_%d", (int)fields_id)!!;

    if (dt.type_param_count > 0) {
        usz params_id = self.next_result();
        char[64] params_name_buf;
        char[] params_name = io::bprintf(&params_name_buf, "_aot_type_params_%d", (int)params_id)!!;
        self.emit_indent();
        self.emit("char[][");
        self.emit_usz(dt.type_param_count);
        self.emit("] ");
        self.emit(params_name);
        self.emit(";\n");
        for (usz i = 0; i < dt.type_param_count; i++) {
            self.emit_indent();
            self.emit(params_name);
            self.emit("[");
            self.emit_usz(i);
            self.emit("] = ");
            self.emit_string_literal(self.interp.symbols.get_name(dt.type_params[i]));
            self.emit(";\n");
        }
        if (dt.field_count > 0) {
            self.emit_indent();
            self.emit("lisp::aot::AotTypeFieldSpec[");
            self.emit_usz(dt.field_count);
            self.emit("] ");
            self.emit(fields_name);
            self.emit(";\n");
            for (usz i = 0; i < dt.field_count; i++) {
                char[96] field_buf;
                char[] field_target = io::bprintf(&field_buf, "%s[%d]", (ZString)fields_name, (int)i)!!;
                self.emit_target_assignment_prefix(field_target, "name");
                self.emit_string_literal(self.interp.symbols.get_name(dt.fields[i].name));
                self.emit(";\n");
                char[128] ann_buf;
                char[] ann_target = io::bprintf(&ann_buf, "%s.annotation", (ZString)field_target)!!;
                self.emit_aot_type_annotation_spec_init(ann_target, &dt.fields[i].type_ann);
            }
        }

        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::define_type(");
        self.emit_string_literal(self.interp.symbols.get_name(dt.name));
        self.emit(", ");
        self.emit_bool_literal(dt.has_parent);
        self.emit(", ");
        self.emit_string_literal(dt.has_parent ? self.interp.symbols.get_name(dt.parent) : "");
        self.emit(", &");
        self.emit(params_name);
        self.emit("[0], ");
        self.emit_usz(dt.type_param_count);
        self.emit(", ");
        if (dt.field_count > 0) {
            self.emit("&");
            self.emit(fields_name);
            self.emit("[0]");
        } else {
            self.emit("null");
        }
        self.emit(", ");
        self.emit_usz(dt.field_count);
        self.emit(");\n");
        return id;
    }

    if (dt.field_count > 0) {
        self.emit_indent();
        self.emit("lisp::aot::AotTypeFieldSpec[");
        self.emit_usz(dt.field_count);
        self.emit("] ");
        self.emit(fields_name);
        self.emit(";\n");
        for (usz i = 0; i < dt.field_count; i++) {
            char[96] field_buf;
            char[] field_target = io::bprintf(&field_buf, "%s[%d]", (ZString)fields_name, (int)i)!!;
            self.emit_target_assignment_prefix(field_target, "name");
            self.emit_string_literal(self.interp.symbols.get_name(dt.fields[i].name));
            self.emit(";\n");
            char[128] ann_buf;
            char[] ann_target = io::bprintf(&ann_buf, "%s.annotation", (ZString)field_target)!!;
            self.emit_aot_type_annotation_spec_init(ann_target, &dt.fields[i].type_ann);
        }
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::define_type(");
    self.emit_string_literal(self.interp.symbols.get_name(dt.name));
    self.emit(", ");
    self.emit_bool_literal(dt.has_parent);
    self.emit(", ");
    self.emit_string_literal(dt.has_parent ? self.interp.symbols.get_name(dt.parent) : "");
    self.emit(", null, 0, ");
    if (dt.field_count > 0) {
        self.emit("&");
        self.emit(fields_name);
        self.emit("[0]");
    } else {
        self.emit("null");
    }
    self.emit(", ");
    self.emit_usz(dt.field_count);
    self.emit(");\n");
    return id;
}
