module lisp;

import main;

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
