module lisp;

import std::io;
import main;

fn usz Compiler.compile_defalias_direct(Compiler* self, Expr* expr) {
    usz ann_id = self.next_result();
    char[64] ann_name_buf;
    char[] ann_name = "_aot_alias_target";
    if (try char[] formatted = io::bprintf(&ann_name_buf, "_aot_alias_target_%d", (int)ann_id)) {
        ann_name = formatted;
    }
    self.emit_indent();
    self.emit("lisp::aot::AotTypeAnnotationSpec ");
    self.emit(ann_name);
    self.emit(";\n");
    self.emit_aot_type_annotation_spec_init(ann_name, &expr.defalias.target);

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::define_alias(");
    self.emit_string_literal(self.interp.symbols.get_name(expr.defalias.name));
    self.emit(", ");
    self.emit("&");
    self.emit(ann_name);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_defeffect_direct(Compiler* self, Expr* expr) {
    char[64] arg_name_buf;
    char[] arg_name = "";
    if (expr.defeffect.has_arg_type) {
        usz ann_id = self.next_result();
        arg_name = "_aot_effect_arg";
        if (try char[] formatted = io::bprintf(&arg_name_buf, "_aot_effect_arg_%d", (int)ann_id)) {
            arg_name = formatted;
        }
        self.emit_indent();
        self.emit("lisp::aot::AotTypeAnnotationSpec ");
        self.emit(arg_name);
        self.emit(";\n");
        self.emit_aot_type_annotation_spec_init(arg_name, &expr.defeffect.arg_type);
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::define_effect(");
    self.emit_string_literal(self.interp.symbols.get_name(expr.defeffect.name));
    self.emit(", ");
    self.emit_bool_literal(expr.defeffect.has_arg_type);
    self.emit(", ");
    if (expr.defeffect.has_arg_type) {
        self.emit("&");
        self.emit(arg_name);
    } else {
        self.emit("null");
    }
    self.emit(");\n");
    SymbolId backing = INVALID_SYMBOL_ID;
    if (self.find_active_module_private_backing(expr.defeffect.name, &backing)) {
        self.emit_indent();
        self.emit_symbol_name(backing);
        self.emit(" = ");
        self.emit_temp_ref(id);
        self.emit(";\n");
    }
    return id;
}
