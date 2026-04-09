module lisp;

import std::io;
import main;

fn void Compiler.emit_defunion_variant_specs(Compiler* self, ExprDefUnion* du, char[] variants_name) {
    if (du.variant_count == 0) return;

    self.emit_indent();
    self.emit("lisp::aot::AotUnionVariantSpec[");
    self.emit_usz(du.variant_count);
    self.emit("] ");
    self.emit(variants_name);
    self.emit(";\n");

    for (usz i = 0; i < du.variant_count; i++) {
        char[96] variant_buf;
        char[] variant_target = compiler_format_generated_name_index(variant_buf[..], variants_name, (long)i);
        self.emit_target_assignment_prefix(variant_target, "name");
        self.emit_string_literal(self.interp.symbols.get_name(du.variants[i].name));
        self.emit(";\n");
        if (du.variants[i].field_count > 0) {
            usz fields_id = self.next_result();
            char[64] fields_name_buf;
            char[] fields_name = compiler_format_generated_name_id(fields_name_buf[..], "_aot_union_variant_fields_", (long)fields_id);
            self.emit_indent();
            self.emit("char[][");
            self.emit_usz(du.variants[i].field_count);
            self.emit("] ");
            self.emit(fields_name);
            self.emit(";\n");
            for (usz fi = 0; fi < du.variants[i].field_count; fi++) {
                self.emit_indent();
                self.emit(fields_name);
                self.emit("[");
                self.emit_usz(fi);
                self.emit("] = ");
                self.emit_string_literal(self.interp.symbols.get_name(du.variants[i].fields[fi]));
                self.emit(";\n");
            }
            self.emit_target_assignment_prefix(variant_target, "fields");
            self.emit("&");
            self.emit(fields_name);
            self.emit("[0];\n");
        } else {
            self.emit_target_assignment_prefix(variant_target, "fields");
            self.emit("null;\n");
        }
        self.emit_target_assignment_prefix(variant_target, "field_count");
        self.emit_usz(du.variants[i].field_count);
        self.emit(";\n");
    }
}
