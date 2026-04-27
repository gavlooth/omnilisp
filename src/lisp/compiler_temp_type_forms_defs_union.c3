module lisp;

import std::io;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (union type definitions)
// =============================================================================
fn usz Compiler.compile_defunion_direct(Compiler* self, Expr* expr) {
    ExprDefUnion* du = expr.defunion;
    usz variants_id = self.next_result();
    char[64] variants_name_buf;
    char[] variants_name = self.generated_name_id(variants_name_buf[..], "_aot_union_variants_", (long)variants_id);

    if (du.type_param_count > 0) {
        usz params_id = self.next_result();
        char[64] params_name_buf;
        char[] params_name = self.generated_name_id(params_name_buf[..], "_aot_union_params_", (long)params_id);
        self.emit_indent();
        self.emit("char[][");
        self.emit_usz(du.type_param_count);
        self.emit("] ");
        self.emit(params_name);
        self.emit(";\n");
        for (usz i = 0; i < du.type_param_count; i++) {
            self.emit_indent();
            self.emit(params_name);
            self.emit("[");
            self.emit_usz(i);
            self.emit("] = ");
            self.emit_string_literal(self.interp.symbols.get_name(du.type_params[i]));
            self.emit(";\n");
        }

        self.emit_defunion_variant_specs(du, variants_name);

        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::define_union(");
        self.emit_string_literal(self.interp.symbols.get_name(du.name));
        self.emit(", &");
        self.emit(params_name);
        self.emit("[0], ");
        self.emit_usz(du.type_param_count);
        self.emit(", ");
        if (du.variant_count > 0) {
            self.emit("&");
            self.emit(variants_name);
            self.emit("[0]");
        } else {
            self.emit("null");
        }
        self.emit(", ");
        self.emit_usz(du.variant_count);
        self.emit(");\n");
        self.emit_helper_published_binding_sync(du.name);
        for (usz vi = 0; vi < du.variant_count; vi++) {
            self.emit_helper_published_binding_sync(du.variants[vi].name);
        }
        return id;
    }

    self.emit_defunion_variant_specs(du, variants_name);

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::define_union(");
    self.emit_string_literal(self.interp.symbols.get_name(du.name));
    self.emit(", null, 0, ");
    if (du.variant_count > 0) {
        self.emit("&");
        self.emit(variants_name);
        self.emit("[0]");
    } else {
        self.emit("null");
    }
    self.emit(", ");
    self.emit_usz(du.variant_count);
    self.emit(");\n");
    self.emit_helper_published_binding_sync(du.name);
    for (usz vi = 0; vi < du.variant_count; vi++) {
        self.emit_helper_published_binding_sync(du.variants[vi].name);
    }
    return id;
}
